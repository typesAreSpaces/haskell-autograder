module HomeworkTests where

import SubmissionHandler
import Grader

-- define the problems and the soultions
homeworkTests = 
  [
  Problem {
    title = "Camping basics",
    description = "In this exercise you will implement basic operations on a dictionary. The dictionary is represented as a list of tuples, each tuple containing a key and a value, for example: '[(key1, value1), (key2, value2), (key3, value3)]'.",
    tests = [
      Test {
        name = "Add an entry - empty dictionary",
        score = 1,
        function = "addEntry ('a', 1) []",
        result = show $ addEntry ('a', 1) [],
        expected = "[('a',1)]"
      },
    Test {
      name = "Add an entry - one entry",
      score = 1,
      function = "addEntry ('a', 1) [('b', 2)]",
      result = show $ addEntry ('a', 1) [('b', 2)],
      expected = "[('a',1),('b',2)]"
    },
    Test {
      name = "Add an entry - many entries",
      score = 1,
      function = "addEntry ('a', 1) [('b', 2), ('c', 3), ('d', 4)]",
      result = show $ addEntry ('a', 1) [('b', 2), ('c', 3), ('d', 4)],
      expected = "[('a',1),('b',2),('c',3),('d',4)]"
    },
    Test {
      name = "Remove newest entry - empty dictionary",
      score = 1,
      function = "removeNewestEntry []",
      result = show $ removeNewestEntry [],
      expected = "[]"
    },
    Test {
      name = "Remove newest entry - one entry",
      score = 1,
      function = "removeNewestEntry [('a',1)]",
      result = show $ removeNewestEntry [('a',1)],
      expected = "[]"
    },
    Test {
      name = "Remove newest entry - many entries",
      score = 1,
      function = "removeNewestEntry [('a',1),('b',2),('c',3),('d',4)]",
      result = show $ removeNewestEntry [('a',1),('b',2),('c',3),('d',4)],
      expected = "[('b',2),('c',3),('d',4)]"
    },
    Test {
      name = "Remove oldest entry - empty dictionary",
      score = 1,
      function = "removeOldestEntry []",
      result = show $ removeOldestEntry [],
      expected = "[]"
    },
    Test {
      name = "Remove oldest entry - one entry",
      score = 1,
      function = "removeOldestEntry [('a',1)]",
      result = show $ removeOldestEntry [('a',1)],
      expected = "[]"
    },
    Test {
      name = "Remove oldest entry - many entries",
      score = 1,
      function = "removeOldestEntry [('a',1),('b',2),('c',3),('d',4)]",
      result = show $ removeOldestEntry [('a',1),('b',2),('c',3),('d',4)],
      expected = "[('a',1),('b',2),('c',3)]"
    },
    Test {
      name = "Get all keys - empty dictionary",
      score = 1,
      function = "getAllKeys []",
      result = show $ getAllKeys [],
      expected = show ""
    },
    Test {
      name = "Get all keys - one entry",
      score = 1,
      function = "getAllKeys [('a',1)]",
      result = show $ getAllKeys [('a',1)],
      expected = show "a"
    },
    Test {
      name = "Get all keys - many entries",
      score = 1,
      function = "getAllKeys [('a',1),('b',2),('c',3),('d',4)]",
      result = show $ getAllKeys [('a',1),('b',2),('c',3),('d',4)],
      expected = show "abcd"
    },
    Test {
      name = "Get all values - empty dictionary",
      score = 1,
      function = "getAllValues []",
      result = show $ getAllValues [],
      expected = "[]"
    },
    Test {
      name = "Get all values - one entry",
      score = 1,
      function = "getAllValues [('a',1)]",
      result = show $ getAllValues [('a',1)],
      expected = "[1]"
    },
    Test {
      name = "Get all values - many entries",
      score = 1,
      function = "getAllValues [('a',1),('b',2),('c',3),('d',4)]",
      result = show $ getAllValues [('a',1),('b',2),('c',3),('d',4)],
      expected = "[1,2,3,4]"
    }
    ]
  },
  Problem {
    title = "Navigating in the woods",
    description = "More exercises featuring operations on dictionaries. You should be concerned about duplicated entries (entries having the same keys) only when implementing the last function.",
    tests = [
      Test {
        name = "Remove entry with key - empty dictionary",
        score = 1,
        function = "removeEntry 'a' []",
        result = show $ removeEntry 'a' [],
        expected = "[]"
      },
    Test {
      name = "Remove entry with key - key not found",
      score = 1,
      function = "removeEntry 'f' [('a',1),('b',2),('c',3),('d',4)]",
      result = show $ removeEntry 'f' [('a',1),('b',2),('c',3),('d',4)],
      expected = "[('a',1),('b',2),('c',3),('d',4)]"
    },
    Test {
      name = "Remove entry with key - key found",
      score = 1,
      function = "removeEntry 'c' [('a',1),('b',2),('c',3),('d',4)]",
      result = show $ removeEntry 'c' [('a',1),('b',2),('c',3),('d',4)],
      expected = "[('a',1),('b',2),('d',4)]"
    },
    Test {
      name = "Search entry with key - empty dictionary",
      score = 1,
      function = "searchEntry 'a' []",
      result = show $ searchEntry 'a' [],
      expected = "Nothing"
    },
    Test {
      name = "Search entry with key - key not found",
      score = 1,
      function = "searchEntry 'f' [('a',1),('b',2),('c',3),('d',4)]",
      result = show $ searchEntry 'f' [('a',1),('b',2),('c',3),('d',4)],
      expected = "Nothing"
    },
    Test {
      name = "Search entry with key - key found",
      score = 1,
      function = "searchEntry 'c' [('a',1),('b',2),('c',3),('d',4)]",
      result = show $ searchEntry 'c' [('a',1),('b',2),('c',3),('d',4)],
      expected = "Just 3"
    },
    Test {
      name = "Remove entries with duplicate keys - empty dictionary",
      score = 1,
      function = "optimize []",
      result = show $ optimize [],
      expected = "[]"
    },
    Test {
      name = "Remove entries with duplicate keys - no duplicate keys",
      score = 1,
      function = "optimize [('a',1),('b',2),('c',3),('d',4)]",
      result = show $ optimize [('a',1),('b',2),('c',3),('d',4)],
      expected = "[('a',1),('b',2),('c',3),('d',4)]"
    },
    Test {
      name = "Remove entries with duplicate keys - duplicate entries",
      score = 1,
      function = "optimize [('a',1),('a',1),('b',2),('a',1),('b',2),('c',3),('d',4),('a',1),('d',4),('d',4)]",
      result = show $ optimize [('a',1),('a',1),('b',2),('a',1),('b',2),('c',3),('d',4),('a',1),('d',4),('d',4)],
      expected = "[('a',1),('b',2),('c',3),('d',4)]"
    },
    Test {
      name = "Remove entries with duplicate keys - duplicate keys",
      score = 1,
      function = "optimize [('a',1),('a',3),('b',2),('a',2),('b',1),('c',3),('d',4),('a',2),('d',3),('d',3)]",
      result = show $ optimize [('a',1),('a',3),('b',2),('a',2),('b',1),('c',3),('d',4),('a',2),('d',3),('d',3)],
      expected = "[('a',1),('b',2),('c',3),('d',4)]"
    }
    ]
  },
  Problem {
    title = "Two maps, we are lost",
    description = "In this exercise you will work with two dictionaries at once. Each dictionary can be considered as well-formed, they do not contain entries with the same key. However by working with two dictionaries at once you may find conflicting entries, entries which have the same key. Each implemented function will manage these conflicts differently. First, you will write the function subtract, which will remove all the conflicting entries from the first dictionary. Next, you will implement the merge function, which will add the left dictionary to the not conflicting entries of the second dictionary (keeps the conflicting entries from the first dictionary). Finally, you will implement merge with, which will use a function given as a parameter to decide the kept value for conflicting entries.",
    tests = [
      Test {
        name = "Subtract the second dictionary from the first - common entities",
        score = 1,
        function = "subtractDictionary [('a',1),('b',2),('c',3),('d',4),('e',5)] [('d',4),('e',5),('f',6),('g',7)]",
        result = show $ subtractDictionary [('a',1),('b',2),('c',3),('d',4),('e',5)] [('d',4),('e',5),('f',6),('g',7)],
        expected = "[('a',1),('b',2),('c',3)]"
      },
    Test {
      name = "Subtract the second dictionary from the first - common keys",
      score = 1,
      function = "subtractDictionary [('a',1),('b',2),('c',3),('e',5),('d',4)] [('d',6),('e',7),('f',8),('g',9)]",
      result = show $ subtractDictionary [('a',1),('b',2),('c',3),('e',5),('d',4)] [('d',6),('e',7),('f',8),('g',9)],
      expected = "[('a',1),('b',2),('c',3)]"
    },
    Test {
      name = "Merge two dictionaries, keep conflicting entities from first - common entities",
      score = 1,
      function = "mergeDictionary [('a',1),('b',2),('c',3),('d',4),('e',5)] [('d',4),('e',5),('f',6),('g',7)]",
      result = show $ mergeDictionary [('a',1),('b',2),('c',3),('d',4),('e',5)] [('d',4),('e',5),('f',6),('g',7)],
      expected = "[('a',1),('b',2),('c',3),('d',4),('e',5),('f',6),('g',7)]"
    },
    Test {
      name = "Merge two dictionaries, keep conflicting entities from first - common keys",
      score = 1,
      function = "mergeDictionary [('a',1),('b',2),('c',3),('d',4),('e',5)] [('e',7),('d',6),('f',8),('g',9)]",
      result = show $ mergeDictionary [('a',1),('b',2),('c',3),('d',4),('e',5)] [('e',7),('d',6),('f',8),('g',9)],
      expected = "[('a',1),('b',2),('c',3),('d',4),('e',5),('f',8),('g',9)]"
    },
    Test {
      name = "Merge two dictionaries, solve conflicts with a function - keep greater",
      score = 1,
      function = "mergeDictionaryWith max [('a',1),('b',2),('c',3),('d',13),('e',12)] [('d',14),('e',11),('f',8),('g',9)]",
      result = show $ mergeDictionaryWith max [('a',1),('b',2),('c',3),('d',13),('e',12)] [('d',14),('e',11),('f',8),('g',9)],
      expected = "[('a',1),('b',2),('c',3),('d',14),('e',12),('f',8),('g',9)]"
    },
    Test {
      name = "Merge two dictionaries, solve conflicts with a function - keep smaller",
      score = 1,
      function = "mergeDictionaryWith min [('a',1),('b',2),('c',3),('e',13),('d',12)] [('d',14),('e',11),('f',8),('g',9)]",
      result = show $ mergeDictionaryWith min [('a',1),('b',2),('c',3),('e',13),('d',12)] [('d',14),('e',11),('f',8),('g',9)],
      expected = "[('a',1),('b',2),('c',3),('e',11),('d',12),('f',8),('g',9)]"
    }
    ]
  },
  Problem {
    title = "Being asked by a kind traveler",
    description = "There is always time to chat with an NPC just to get another worthless side quest on your map.",
    tests = [
      Test {
        name = "Hey you! Where are you going?",
        score = 1,
        function = "Your answer",
        result = yourAnswer,
        expected = yourAnswer
      }
    ]
  } 
  ]
