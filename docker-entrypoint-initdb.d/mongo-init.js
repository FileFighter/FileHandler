print("Started Adding the Users.");
db = db.getSiblingDB("filehandler");
print("db:" , db);
db.createUser({
  user: "filehandler",
  pwd: "example",
  roles: [{ role: "readWrite", db: "filehandler" }],
});
print("End Adding the User Roles.");
