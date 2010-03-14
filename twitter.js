
function get_content_type(filepath)
{
    var file_ending = filepath.extension();
    var content = "Content-Type: ";
    if (file_ending.match(/\.txt$/)) {
	content = content + "text/plain";
    } 
}

function stream_twitter()
{
    var client = Titanium.Network.createHTTPClient();
    client.addEventListener(client.HTTP_DATA_RECEIVED, 
			    function(data) {
			    });
    client.open("GET", "http://stream.twitter.com/1/statuses/sample.json", true, "", "");
}
