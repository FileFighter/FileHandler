# FileHandlerService

Haskell FileHandler Server.

_Work In Progress_

Base of this code base is this [repo](https://github.com/snoyberg/file-server-demo)

## Features
- [ ] browse does not exist anymore.
- [ ] upload path is POST /upload?id=id,id1,id2&token=token
- [ ] request to upload triggers request to backend
- [ ] upload does support multiple files
- [ ] download path is GET /download?id=id,id1,id2&token=token
- [ ] request to download triggers request to backend
- [ ] download supports multiple files (zipped as one)
- [ ] service is either mapped with a usefull prefix /userdata/ or a fake subdomain files.....de/upload...  
**(Roadmap feature)**
- [ ] there is another path /preview/id?token=token

Text below is from the original code base.

---

# Getting started

`stack build --file-watch --watch-all  --fast`

`filewatcher --restart '**/*.hs' 'stack build --fast && stack exec Filehandler-exe'`

`stack exec Filehandler-exe`
