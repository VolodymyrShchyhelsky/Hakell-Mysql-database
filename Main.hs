import SchoolDataBaseModule

main = do 
      database_connection <- connectToDb
      getTable database_connection
