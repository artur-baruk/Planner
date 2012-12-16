-- Type describing a room in a building
data Room = Room { room_number::Int
					} deriving (Eq, Show)

-- Type describing a group of students					
data Group = Group { group_id::Int
					} deriving (Eq, Show)

-- Type describing a subject					
data Subject = Subject { subject_id::Int
						, name::String
						} deriving (Eq, Show)

data Day = Mon | Tue | Wed | Thu | Fri deriving (Eq, Show)		

-- Time as an hour and minutes
data Time = Time Int Int deriving (Eq, Show) -- we need to implement an order interface Ord somehow to be able to compare time 
						
-- Type matching a room, a group and a subject 						
data Class = Class { room::Room
					, group::Group
					, subject::Subject
					, day::Day
					, startTime::Time
					, endTime::Time
					} deriving (Eq, Show)

-- Calculates minutes between two timestamps, the first timestamp must be greater or equal to the second					
timeDiff :: Time -> Time -> Int
timeDiff (Time hour_1 minute_1) (Time hour_2 minute_2) = hour_1*60 + minute_1 - hour_2*60 - minute_2

