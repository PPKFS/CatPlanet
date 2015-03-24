%
% The main city.
%

dig('Living Room', LivingRoom, [description('A sitting room.'), exit(north, Bathroom, Livingroom_Bathroom)]).
dig('Bathroom', Bathroom, [description('A bathroom.')]).