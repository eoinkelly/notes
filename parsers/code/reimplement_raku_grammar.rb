# Status: just a sketch of ideas

# could be inherited or a module include or composition - i'm doing inheritance for now
# grammars are bags of rules
 class Grammar
   # @param [String] str  The string to be parsed
   # @param [?] actions The actions object to run on each matching token/rule
   def self.parse(str, actions: nil)
   end
 end

 class Action
 end

 # i think token is just  a rule which ignores whitespace ?

 # grammar = rules + tokens + regexes
 #
 # each method returns either a regex, token or rule
 class RestGrammar < Grammar

   # default first token attempted to match when parsing
   # returns a regex or a referenc to another otken
   def top_token
     "/<subject>/<command>/<data>"
   end

   def subject_token
     /\w+/
   end

   def command_token
     /\w+/
   end

   def data_token
     /.*/
   end
 end


 class MyAction
   # each method here gets a match object from the corresponding grammar and returns a match object
   # these methods are invoked by the parser to transform the output of each token before ???
   #    before breaking down further?
   #    before generating output?
 end

 # a match is very hash like
 class Match
   def to_s
     "become str"
   end
 end

 ex_1 = "/product/update/7/notify"
 p top_match = RestGrammar.parse(ex_1)


 # each of these return a match object
 top_match[:subject]
 top_match[:command]
 top_match[:data]

 # or
 top_match.subject
 top_match.command
 top_match.data
