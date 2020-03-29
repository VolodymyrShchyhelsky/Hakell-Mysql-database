{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-} 
{-# LANGUAGE UndecidableInstances #-} 

import Database.MySQL.Base
import qualified System.IO.Streams as Streams
import Data.Binary.Put
import Data.Binary.Get
import qualified Data.ByteString.Lazy as LazyByte
import qualified Data.ByteString as B
import qualified Data.Text as DataText
import qualified Data.Text.Encoding as Encoder
import GHC.Generics
import qualified Data.Map as Map
import qualified Data.String as DataString
import qualified Data.List as DataList
import Control.Exception
import Text.PrettyPrint.Boxes


------------------ TABLE PRINT --------------------------

pad width x = x ++ replicate k ' '
      where k = width - length x

fmt_column :: [String] -> Box
fmt_column items = hsep // vcat left (DataList.intersperse hsep (map (text.pad width) items)) // hsep
      where width = maximum $ map length items
            hsep = text ( replicate width '-' )

tableBoxPrint :: [[String]] -> Box
tableBoxPrint rows = vsep Text.PrettyPrint.Boxes.<> hcat top (DataList.intersperse vsep (map fmt_column columns)) Text.PrettyPrint.Boxes.<> vsep
      where
            columns = DataList.transpose rows
            nrows = length rows
            vsep =  vcat left $ map char ("+" ++ (concat $ replicate nrows "|+"))

printTable :: (TableHolder a) => a -> IO()
printTable table = do
      let out = column_list (getData table) : row_list (getData table)
      putStrLn $ Text.PrettyPrint.Boxes.render $ tableBoxPrint out
      return ()

---------------- TABLE PRINT --------------------------


getStringFromByteStr :: B.ByteString -> String
getStringFromByteStr strict_byteStr = str
      where text = Encoder.decodeUtf8 strict_byteStr
            str = DataText.unpack text

getStringFromRecord :: MySQLValue -> String
getStringFromRecord value = str
      where byteStr = runPut (putTextField value)
            strict_byteStr = LazyByte.toStrict byteStr
            str = getStringFromByteStr strict_byteStr

getColumnName :: ColumnDef -> String
getColumnName column = getStringFromByteStr (columnName column)

getColumnNames :: [ColumnDef] -> [String]
getColumnNames columns = map getColumnName columns

getRowValues :: [MySQLValue] -> [String]
getRowValues row = map getStringFromRecord row

getRowsValues :: [[MySQLValue]] -> [[String]]
getRowsValues rows = map getRowValues rows

class TableHolder a where
      getData :: a -> Table
      setData :: Table -> a
      checkUpdateValues :: a -> [String] -> MySQLConn -> String -> IO Bool
      checkInsertValues :: a -> [String] -> MySQLConn -> IO Bool
      printEnums :: a -> IO ()
      prepareValuesToQuery :: a -> [String] -> [String]

data Table = Table
      { name :: String
      -- , columns :: [ColumnDef]
      -- , sql_rows :: [[MySQLValue]]
      , column_list :: [String]
      , row_list :: [[String]]
      }

get_id_check :: String -> String
get_id_check "" = ""
get_id_check id = " and id <> " ++ id

putBraces :: String -> String
putBraces value = ("\"" ++ value ++ "\"")

replaceNth :: Int -> [String] -> [String]
replaceNth _ [] = []
replaceNth n (x:xs)
      | n == 0 = newVal:xs
      | otherwise = x:replaceNth (n-1) xs
      where newVal = (putBraces x)

----------------- SEATS TABLE 

data SeatsTable = SeatsTable
      { seats_table_data :: Table
      }

changeSeatsTable :: [String] -> MySQLConn -> String -> IO Bool
changeSeatsTable values conn id = do
      let capacity_q = "SELECT capacity FROM rooms where id = " ++ (values !! 0)
      (c, r_io) <- query_ conn (DataString.fromString $ capacity_q)
      r <- Streams.toList r_io
      let row_list = (getRowsValues r)
      let capacity = (read (row_list !! 0 !! 0) :: Int)
      let id_check = (get_id_check id)
      let count_q = "SELECT * FROM seats where room_id = " ++ (values !! 0) ++ id_check
      (c, r_io) <- query_ conn (DataString.fromString $ count_q)
      r <- Streams.toList r_io
      let count = (length r)
      if count == capacity
            then return False
            else return True

instance TableHolder SeatsTable where
      getData t = (seats_table_data t)
      setData table = SeatsTable {seats_table_data = table}
      checkUpdateValues t values conn id = (changeSeatsTable values conn id)
      checkInsertValues t values conn = (changeSeatsTable values conn "")
      printEnums t = return ()
      prepareValuesToQuery t values = values

----------------- SEATS TABLE 




------------------ ROOMS TABLE 
data RoomsTable = RoomsTable 
      { rooms_table_data :: Table
      }

instance TableHolder RoomsTable where
      getData t = (rooms_table_data t)
      setData table = RoomsTable {rooms_table_data = table}
      printEnums t = return ()
      checkInsertValues t values conn = return True

      prepareValuesToQuery t values = values

      checkUpdateValues t values conn id = do
            let count_q = "SELECT * FROM seats where room_id = " ++ id
            (c, r_io) <- query_ conn (DataString.fromString $ count_q)
            r <- Streams.toList r_io
            let count = (length r)
            let capacity = (read  (values !! 0) :: Int)
            if count > capacity
                  then return False
                  else return True
      
------------------- ROOMS TABLE



-------------------- LESSONS TABLE
changeLessonsTable :: [String] -> MySQLConn -> String -> IO Bool
changeLessonsTable values conn id = do
      let teacher_id_q = "SELECT teacher_id FROM faculty_classes WHERE id = " ++ (values !! 0)
      (c, r_io) <- query_ conn (DataString.fromString $ teacher_id_q)
      r <- Streams.toList r_io
      if length r == 0
            then return True
            else do
                  let teacher_id = (getRowsValues r) !! 0 !! 0
                  let id_check = (get_id_check id)
                  let count_q = "SELECT COUNT(*) FROM lessons \
                                    \JOIN faculty_classes ON lessons.class_id = faculty_classes.id \
                                    \AND faculty_classes.teacher_id = " ++ teacher_id ++ " AND day_of_week = " ++ (values !! 2) ++ " AND time_slot = " ++ (values !! 3) ++ id_check
                  
                  (c, r_io) <- query_ conn (DataString.fromString $ count_q)
                  r <- Streams.toList r_io
                  let answer = (getRowsValues r) !! 0 !! 0 
                  if answer /= "0"
                        then do
                              print "Sorry there is another lesson in the same time with this teacher"
                              return False
                        else return True

data LessonsTable = LessonsTable 
      { lessons_table_data :: Table
      }

instance TableHolder LessonsTable where
      getData t = (lessons_table_data t)
      setData table = LessonsTable {lessons_table_data = table}
      checkUpdateValues t values conn id = (changeLessonsTable values conn id)
      checkInsertValues t values conn = (changeLessonsTable values conn "")

      prepareValuesToQuery t values = (replaceNth 2 values1)
            where values1 = (replaceNth 3 values)

      printEnums t = do
            putStrLn "day_of_week enums : Monday, Tuesday, Wednesday, Thursday, Friday, Saturday, Sunday"
            putStrLn "time_slot enums : 7-8, 8-9, 9-10, 10-11, 11-12, 12-13, 13-14, 14-15, 15-16, 16-17, 17-18, 18-19, 19-20"
-------------------- LESSONS TABLE



---------------------- FACULTY USERS TABLE
data FacultyUsersTable = FacultyUsersTable 
      { faculty_users_table_data :: Table
      }

instance TableHolder FacultyUsersTable where
      getData t = (faculty_users_table_data t)
      setData table = FacultyUsersTable {faculty_users_table_data = table}
      checkUpdateValues t values conn id = return True
      checkInsertValues t values conn = return True
      printEnums t = do
            putStrLn "status enums: Teacher, Student"
      prepareValuesToQuery t values = upd_values
            where upd_values = (replaceNth 0 values1)
                  values1 = (replaceNth 1 values)
---------------------- FACULTY USERS TABLE





---------------------- FACULTY CLASSES TABLE
data FacultyClassesTable = FacultyClassesTable 
      { faculty_classes_table_data :: Table
      }

instance TableHolder FacultyClassesTable where
      getData t = (faculty_classes_table_data t)
      setData table = FacultyClassesTable {faculty_classes_table_data = table}
      printEnums t = return ()
      checkInsertValues t values conn = return True
      checkUpdateValues t values conn id = do
            let teacher_lesson_count_q = "SELECT COUNT(*) = 0 FROM \
                                          \(SELECT COUNT(*) lessons_at_the_same_time FROM \
                                          \(SELECT class_id, day_of_week, time_slot FROM lessons \
                                          \WHERE class_id = " ++ id ++ " UNION \
                                          \(SELECT lessons.class_id, day_of_week, time_slot FROM lessons \
                                          \JOIN faculty_classes ON lessons.class_id = faculty_classes.id AND teacher_id = " ++ (values !! 0) ++ ")) \
                                          \all_lessons GROUP BY time_slot, day_of_week) lessons_at_the_same_time_count \
                                          \WHERE lessons_at_the_same_time > 1"
            (c, r_io) <- query_ conn (DataString.fromString $ teacher_lesson_count_q)
            r <- Streams.toList r_io
            let teacher_lesson_count = (getRowsValues r) !! 0 !! 0
            if teacher_lesson_count /= "1"
            then do 
                  putStrLn "ERROR: Can't have several lessons at the same time."
                  return False
            else return True

      prepareValuesToQuery t values = (replaceNth 1 values)
---------------------- FACULTY CLASSES TABLE




----------------------- USER TO TABLE LESSON
data UserToLessonTable = UserToLessonTable 
      { user_to_lesson_table_data :: Table
      }

instance TableHolder UserToLessonTable where
      getData t = (user_to_lesson_table_data t)

      setData table = UserToLessonTable {user_to_lesson_table_data = table}

      printEnums t = return ()

      prepareValuesToQuery t values = values

      checkUpdateValues t values conn id = (checkInsertValues t values conn)

      checkInsertValues t values conn = do
            let lesson_room_id_q = "SELECT room_id FROM lessons WHERE id = " ++ (values !! 0)
            (c, r_io) <- query_ conn (DataString.fromString $ lesson_room_id_q)
            r <- Streams.toList r_io
            let lesson_room_id = (getRowsValues r) !! 0 !! 0
            let seet_room_id_q = "SELECT room_id FROM seats WHERE id = " ++ (values !! 2)
            (c, r_io) <- query_ conn (DataString.fromString $ seet_room_id_q)
            r <- Streams.toList r_io
            let seet_room_id = (getRowsValues r) !! 0 !! 0
            print "ROOMS IDS"
            print lesson_room_id
            print seet_room_id
            if lesson_room_id /= seet_room_id
                  then do
                        print "ERROR: The seat is not in the room where the lesson takes place."
                        return False
                  else do
                        let is_user_have_another_lesson_q = "SELECT COUNT(*) = 0 FROM \
                                                \(SELECT day_of_week, time_slot FROM users_to_lessons \
                                                \JOIN lessons ON users_to_lessons.lesson_id = lessons.id \
                                                \WHERE users_to_lessons.user_id = " ++ (values !! 1) ++ ") AS busy_time \
                                                \JOIN \
                                                \(SELECT day_of_week, time_slot FROM lessons \
                                                \WHERE lessons.id = " ++ (values !! 0) ++ ") AS new_lesson \
                                                \ON busy_time.day_of_week = new_lesson.day_of_week AND busy_time.time_slot = new_lesson.time_slot" 
                        (c, r_io) <- query_ conn (DataString.fromString $ is_user_have_another_lesson_q)
                        r <- Streams.toList r_io
                        let is_user_have_another_lesson = (getRowsValues r) !! 0 !! 0
                        if is_user_have_another_lesson == "False"
                              then do
                                    print "User have another lesson"
                                    return False
                              else return True

----------------------- USER TO TABLE LESSON



readRowFieldInput :: String -> IO String
readRowFieldInput name = do
      print (name)
      value_io <- getLine
      return value_io

readFieldInput :: [String] -> IO [String]
readFieldInput (a:b:c) = do
      str <- readRowFieldInput a
      other_str <- readFieldInput (b:c)
      return (str : other_str)
readFieldInput (a:_) = do
      str <- readRowFieldInput a
      return [str]

readTable :: (TableHolder a) => MySQLConn -> a -> IO a
readTable conn t = do
      let select_all = "SELECT * FROM " ++ (name (getData t))
      (c, r_io) <- query_ conn (DataString.fromString $ select_all)
      r <- Streams.toList r_io
      let table_updated = (getData t) {row_list = (getRowsValues r), column_list = (getColumnNames c)}
      return (setData table_updated)


executeQuery :: MySQLConn -> String -> IO()
executeQuery conn q = do
      result <- try(execute_  conn (DataString.fromString $ q)) :: IO (Either ERRException OK)
      case result of
            Left err -> putStrLn $ "Fail " ++ show (err)
            otherwise -> putStrLn "Success"

insertTable :: (TableHolder a) => MySQLConn -> a -> [String] -> IO ()
insertTable conn table values = do
      let field_list = DataList.intercalate ", " (tail (column_list (getData table)))
      let value_list = DataList.intercalate ", " values
      let inser_query_str = "INSERT INTO " ++ (name (getData table)) ++ "(" ++ field_list ++ ") VALUES (" ++ value_list ++")"
      executeQuery conn inser_query_str

makeUpdatePair :: String -> String -> String
makeUpdatePair f v = f ++ " = " ++ v

makeUpdateList :: [String] -> [String] -> [String]
makeUpdateList (f1:f2:f3) (v1:v2:v3) = (makeUpdatePair f1 v1) : (makeUpdateList (f2:f3) (v2:v3))
makeUpdateList (f1:_) (v1:_) = [makeUpdatePair f1 v1]

updateTable :: (TableHolder a) => MySQLConn -> a -> [String] -> String -> IO ()
updateTable conn table values id = do
      let field_list = tail (column_list (getData table))
      let update_list = makeUpdateList field_list values
      let update_list_str = DataList.intercalate ", " update_list
      let q = "UPDATE " ++ (name (getData table)) ++ " SET " ++ update_list_str ++ " WHERE (id = " ++ id ++ ")"
      executeQuery conn q

deleteTable :: (TableHolder a) => MySQLConn -> a -> IO ()
deleteTable conn table = do
      putStrLn "Enter id"
      id <- getLine
      let query_str = "DELETE FROM " ++ (name (getData table)) ++ " WHERE (id = " ++ id ++ ")"
      executeQuery conn query_str

updateRow :: (TableHolder a) => MySQLConn -> a -> IO()
updateRow conn t = do
      printEnums t
      putStrLn "Enter id"
      id <- getLine
      putStrLn "Enter new values"
      let col_l = tail (column_list (getData t))
      read_values <- readFieldInput col_l
      let values = (prepareValuesToQuery t read_values)
      check_values <- (checkUpdateValues t values conn id)
      if check_values
            then updateTable conn t values id
            else putStrLn "Fail Insert"

createRow :: (TableHolder a) => MySQLConn -> a -> IO()
createRow conn t = do
      printEnums t
      putStrLn "Enter values"
      let col_l = tail (column_list (getData t))
      read_values <- readFieldInput col_l
      let values = (prepareValuesToQuery t read_values)
      check_values <- (checkInsertValues t values conn)
      if check_values
            then insertTable conn t values
            else putStrLn "Fail create"

connectToDb :: IO MySQLConn
connectToDb = do
      conn <- connect
            defaultConnectInfo {ciUser = "root", ciPassword = "vovondik2536", ciDatabase = "haskell_lab1"}
      return conn

getAction :: (TableHolder a) => MySQLConn -> a -> IO()
getAction conn empty_table = do
      table <- readTable conn empty_table
      putStrLn "__________________________________________"
      putStrLn "Print -  1"
      putStrLn "Insert - 2"
      putStrLn "Delete - 3"
      putStrLn "Update - 4"
      putStrLn "Return - 5"
      action_num_in <- getLine
      let action_num = (read action_num_in :: Int)
      case action_num of
            1 -> do printTable table
            2 -> do createRow conn table
            3 -> do deleteTable conn table
            4 -> do updateRow conn table
            _ -> do getTable conn
      getAction conn table

getTable :: MySQLConn -> IO ()
getTable conn = do
      let tables = ["lessons", "rooms", "seats", "faculty_users", "faculty_classes", "users_to_lessons"]
      putStrLn "__________________________________________"
      putStrLn "lessons - 1"
      putStrLn "rooms - 2"
      putStrLn "seats - 3"
      putStrLn "faculty_users - 4"
      putStrLn "faculty_classes - 5"
      putStrLn "users_to_lessons - 6"
      putStrLn "Enter number of the table"
      table_num_in <- getLine
      let table_num = (read table_num_in :: Int)
      let table_name = tables !! (table_num - 1)
      let tbl_data = Table {name = table_name, column_list = [], row_list = []}
      case table_num of
            1 -> do 
                        getAction conn LessonsTable {lessons_table_data = tbl_data}
            2 -> do
                        getAction conn RoomsTable {rooms_table_data = tbl_data}
            3 -> do
                        getAction conn SeatsTable {seats_table_data = tbl_data}
            4 -> do 
                        getAction conn FacultyUsersTable {faculty_users_table_data = tbl_data}
            5 -> do
                        getAction conn FacultyClassesTable {faculty_classes_table_data = tbl_data}
            6 -> do
                        getAction conn UserToLessonTable {user_to_lesson_table_data = tbl_data}
            _ -> do 
                        getTable conn
      

main = do 
      database_connection <- connectToDb
      getTable database_connection
