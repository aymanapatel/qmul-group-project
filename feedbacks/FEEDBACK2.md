I’ve had a chat with Edmund again today about your project. It is clear that you and your team member have worked seriously, and produced a good deal of code. But there are some points which indicate you still need to understand requirements and standard methodology a bit better. You have worked hard and shown they can write clear code, but the actual logic of the code is often not well-structured. 

The search functionality mentioned is a case in point. In the code there are multiple files dealing with search (itself an issue) but the functions in these files typically combine: 

 - user interaction to get the basic search parameters
 - the search computation itself
 - presentation of the results to the user

This is not how things are supposed to be. The search computation should be separated into a separate function. The user interactions should be kept in a user interface file. None of this has to do with decisions about the user experience, it is how the supporting code is structured. 

There is another issue with the database. There are three tables, roads, road_status_logs and road_disruptions. road_status_logs references roads correctly using a foreign key, but road_disruptions does not. 

Finally, there are some smaller issues: the code submitted does not follow the standard stack project architecture with a separate app and library (in src). All the code is in src. And the name of the project is not as requested. These were not penalised but they do contribute to the overall impression of not learning and using standard structures.