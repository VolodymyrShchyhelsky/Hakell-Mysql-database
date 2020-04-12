import SchoolDataBaseModule
import Test.Tasty         (defaultMain, testGroup)
import Test.Tasty.HUnit   (assertBool, testCase)

main = defaultMain tests

tests = testGroup "Database tests" [createRoom, updateRoom, exceedRoomCapacity, deleteRoom]

createRoom = 
    testCase "Create Room : test of table read and create function" $ do
        conn <- connectToDb
        let tbl_data = Table {name = "rooms", column_list = [], row_list = []}
        table <- readTable conn RoomsTable {rooms_table_data = tbl_data}
        let rows_before = length (row_list (getData table))
        let capacity = ["2"]
        insertTable conn table capacity
        table <- readTable conn RoomsTable {rooms_table_data = tbl_data}
        let rows_now = length (row_list (getData table))
        assertBool "Insert to room table fail" (rows_before + 1 == rows_now)

updateRoom =
    testCase "Update Room" $ do
        conn <- connectToDb
        let tbl_data = Table {name = "rooms", column_list = [], row_list = []}
        table <- readTable conn RoomsTable {rooms_table_data = tbl_data}
        let first_row = (row_list (getData table) !! 0)
        let id = (first_row !! 0)
        -- let id = "1"
        let capacity = ["1"]
        updateTable conn table capacity id
        table <- readTable conn RoomsTable {rooms_table_data = tbl_data}
        let capacity_after_update = (row_list (getData table) !! 0 !! 1)
        assertBool "Update room table fail" (capacity_after_update == "1")

exceedRoomCapacity = 
    testCase "Create seats in room more than room capacity" $ do
        conn <- connectToDb
        let tbl_data = Table {name = "seats", column_list = [], row_list = []}
        seats_table <- readTable conn SeatsTable {seats_table_data = tbl_data}

        let tbl_data = Table {name = "rooms", column_list = [], row_list = []}
        rooms_table <- readTable conn RoomsTable {rooms_table_data = tbl_data}
        let first_row = (row_list (getData rooms_table) !! 0)
        let room_id = (first_row !! 0)

        insertTable conn seats_table [room_id]
        ans <- checkInsertValues seats_table [room_id] conn
        assertBool "Room capacity exceeded" (ans == False)

deleteRoom = 
    testCase "Delete room" $ do
        conn <- connectToDb
        let tbl_data = Table {name = "rooms", column_list = [], row_list = []}
        table <- readTable conn RoomsTable {rooms_table_data = tbl_data}
        let first_row = (row_list (getData table) !! 0)
        let first_room_id_before = (first_row !! 0)
        deleteTable conn table first_room_id_before
        table <- readTable conn RoomsTable {rooms_table_data = tbl_data}
        if length (row_list (getData table)) /= 0 
            then do
                let first_row = (row_list (getData table) !! 0)
                let first_room_id_after = (first_row !! 0)
                assertBool "Room delete fail" (first_room_id_after /= first_room_id_before)
            else assertBool "All fine here" True 