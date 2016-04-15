require 'sinatra'
require 'json'
require 'open3'
def syscall(*cmd)
  begin
    stdout, stderr, status = Open3.capture3(*cmd)
    status.success? && stdout.slice!(0..-(1 + $/.size)) # strip trailing eol
  rescue
  end
end

set :port, 8080

post "/handy" do
  request.body.rewind
  data = request.body.read
  syscall("./Main '" + data + "'")
end
