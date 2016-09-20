require 'apnotic'

apns_connection = Apnotic::Connection.new(cert_path: "apns_certificate.pem", cert_pass: "")

def send_apns(token, msg)
  notification       = Apnotic::Notification.new(token)
  notification.alert = msg
  apns_connection.push(notification)
  puts "Notification sent, token=#{token}, msg=#{msg}"
end

at_exit do
  apns_connection.close
end

require 'sinatra'
require 'json'
require 'open3'
require 'net/http'
require 'uri'
require 'base64'
require 'fileutils'

class String
  def strip_trailing_eol
    slice!(0..-(1 + $/.size))
  end
end

set :port, 8080

post "/handy/:action" do
  request.body.rewind
  data = params["action"] + "?" + request.body.read
  puts "data=" + data
  FileUtils.cp("db", "db_bak")
  stdout, status = Open3.capture2("./Main '#{data}'")
  reply_obj = JSON.parse(stdout)  
  ret = reply_obj["reply"]
  notif = reply_obj["notif"]
  puts "ret=#{ret}"
  puts "notif=#{notif}"
  if status.success?
    send_apns(notif['token'], notif['contents']) if notif
  else
    if File.size?("db")
      puts "Main failed but database is not lost, size=#{File.size?("db")}"
    else
      FileUtils.cp("db_bak", "db")
      puts "Database lost, use backup database"
    end
  end
  ret
end

# image server
require 'sqlite3'
db = SQLite3::Database.new "image-rb.db"

def db_init(db)
  db.execute("create table if not exists Book(isbn, title, author, image)")
  db.execute("create table if not exists Item(id, images)")
end

db_init(db)

Book = Struct.new(:title, :author, :isbn, :image)

def google_query_book(isbn)
  querylink = "https://www.googleapis.com/books/v1/volumes?q=isbn:#{isbn}"
  response = Net::HTTP.get_response(URI(querylink))
  reply = JSON.parse(response.body)
  volumeInfo = reply['items'][0]['volumeInfo']
  Book.new(volumeInfo['title'],
           volumeInfo['authors'][0],
           volumeInfo['industryIdentifiers'][0]['identifier'],
           Base64.encode64(Net::HTTP.get_response(URI(volumeInfo['imageLinks']['thumbnail'])).body))
end

# string -> Book
def db_get_book(isbn)
  row = db.get_first_row("select * from Book where isbn = ?", [isbn])
  if row
    return Book.new(*row)
  else
    book = google_query_book(isbn)
    db.execute("insert into Book values(?, ?, ?, ?)", [isbn, book.title, book.author, book.image])
    return book
  end
end

# string -> [string], list of base64-encoded images
def getSellItemImages(id)
  puts "getSellItemImages(id=#{id})"
  row = db.get_first_row("select images from Item where id = ?", [id])
  return eval row
end

# string * [string] (base64-encoded images) -> ()
def postSellItemImages(id, images)
  puts "postSellItemImages(id=#{id}, images=...)"
  db.execute("insert into Item values(?, ?)", id, images.to_s)
end

# string -> string (base64-encoded image)
def getBookCoverImage(isbn)
  puts "getBookCoverImage(isbn=#{isbn})"
  book = db_get_book(isbn)
  return book.image
end

# string -> [string, string]
def queryIsbn(isbn)
  puts "queryIsbn(isbn=#{isbn})"
  book = db_get_book(isbn)
  return [book.title, book.author]
end

post "/images/:action" do
  request.body.rewind
  body_dict = request.body.read
  data = params["action"] + " *" + JSON.parse(body_dict)["args"].to_s
  puts "data=" + data
  ret = eval(data)
  puts "ret=" + ret
  ret
end
