import Database (Database, getDatabase, runMigration)

main :: IO ()
main = getDatabase >>= runMigration
