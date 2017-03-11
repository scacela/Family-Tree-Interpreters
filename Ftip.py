import sys

# Sam Cacela

name = ''
class Person:

    # constructor that gives Person instance a name
    def __init__(self, name):
        self.name = name
        
        '''
        each list within the constructor makes each
        list a property of its respective Person instance
        as opposed to outside the constructor, where children would
        be shared among Person instances and repeated
        '''
        
        self.spouse = []
        self.parent = []
        self.child = []
        self.sibling = []
        self.halfsibling = []
        self.ancestor = []
        self.descendant = []
        self.cousin = []

    # provides String representation of Person intance
    def toStr(self):
        return self.name

# dictionary
d = {}

# name constraints message used in all queries
ncMsg = ('Query entry not registered: a name must\n'
         'contain no hyphen and be less than 20\n'
         'characters long')

lineList = []

for line in sys.stdin:
    if not line.isspace():  # filters out whitespace lines in sys.stdin
        lineList.append(line)
    
for line in lineList:
    
    line_split = line.split()
    
    if line_split[0] is 'E':

        sameName = False
        reincarnation = False
        nameConstraints = False
        numSplits = 0
        for splitCount in range(len(line_split)):
            numSplits+=1

        name1 = line_split[1]
        name2 = line_split[2]

        if (len(name1) > 19 or '-' in name1 or
            len(name2) > 19 or '-' in name2):
            nameConstraints = True

        if name1 != name2:
            # if name1 and nam2 are not in d, give them new object instances
            # and put them in d
            if name1 not in d:
                d[name1] = Person(name1)
            if name2 not in d:
                d[name2] = Person(name2)
        else:
            sameName = True
            
        '''
        SPOUSE
        '''
        if d[name2] not in d[name1].spouse:
            d[name1].spouse.append(d[name2])
        if d[name1] not in d[name2].spouse:
            d[name2].spouse.append(d[name1])

        if numSplits == 3:
            print ('\nE ' + name1 + ' ' + name2)

        if numSplits == 4:
            name3 = line_split[3]
            print ('\nE ' + name1 + ' ' + name2 + ' ' + name3)

            if len(name3) > 19 or '-' in name3:
                nameConstraints = True

            if name3 == name1 or name3 == name2:
                sameName = True
            
            if name3 in d:
                reincarnation = True
            

            if (sameName == False and reincarnation == False and
                nameConstraints == False):

                d[name3] = Person(name3)
                
                '''
                CHILD
                '''
                d[name1].child.append(d[name3])
                d[name2].child.append(d[name3])
                
                '''
                PARENT
                '''
                d[name3].parent.append(d[name1])
                d[name3].parent.append(d[name2])
            
                '''
                SIBLING
                '''
                for i in d[name1].child:
                    if i in d[name2].child:
                        if i != d[name3]:
                            if i not in d[name3].sibling:
                                d[name3].sibling.append(i)
                                i.sibling.append(d[name3])

                '''
                HALF-SIBLING
                '''
                for i in d[name1].child:
                    if i not in d[name2].child:
                        if i != d[name3]:
                            if i not in d[name3].halfsibling:
                                d[name3].halfsibling.append(i)
                                i.halfsibling.append(d[name3])

                '''
                ANCESTOR
                '''
                # first, add parents to child's ancestor list
                d[name3].ancestor.append(d[name1])
                d[name3].ancestor.append(d[name2])

                # and add child to parents' descendant lists
                d[name1].descendant.append(d[name3])
                d[name2].descendant.append(d[name3])

                # recursive method
                # also finds descendants
                def findAncestors(par1, par2, ch):
                    if len(par1.parent) == 2:
                        a = par1.parent[0]
                        b = par1.parent[1]
                            
                        if a not in ch.ancestor:
                            ch.ancestor.append(a)
                            a.descendant.append(ch)
                        if b not in ch.ancestor:
                            ch.ancestor.append(b)
                            b.descendant.append(ch)
                                
                        findAncestors(a, b, ch)

                    if len(par2.parent) == 2:
                        a = par2.parent[0]
                        b = par2.parent[1]
                            
                        if a not in ch.ancestor:
                            ch.ancestor.append(a)
                            a.descendant.append(ch)
                        if b not in ch.ancestor:
                            ch.ancestor.append(b)
                            b.descendant.append(ch)
                                
                        findAncestors(a, b, ch)

                # trace lineage as far back as possible
                findAncestors(d[name1], d[name2], d[name3])
                
                for cous in d:
                    for cmAnc in d[cous].ancestor:
                        if cmAnc in d[name3].ancestor:
                            if d[cous] not in d[name3].cousin:
                                if cous is not name3:
                                    d[name3].cousin.append(d[cous])
                                    d[cous].cousin.append(d[name3])
                                    

        if sameName:
            snMsg = ('Query entry not registered: no two\n'
                     'names provided may be"the same')
            print (snMsg)
            
        if reincarnation:
            rcMsg = ('Query entry not registered:\n'
                     + name3 + ' was already born.')
            print (rcMsg)

        if nameConstraints:
            print (ncMsg)


    if line_split[0] is 'R':
        name1 = line_split[1]
        name2 = line_split[2]

        print ('\nR ' + name1 + ' ' + name2)

        if (len(name1) > 19 or '-' in name1 or
            len(name2) > 19 or '-' in name2):
            print (ncMsg)

        elif name1 == name2:
            print ('A person has no relation to his '
                   'or herself in this program.')

        elif name1 not in d or name2 not in d:
            print (name1 + ' and ' + name2 + ' are '
                   'unrelated')

        elif d[name1] in d[name2].spouse:
            print (name1 + ' is married to ' + name2)
        elif d[name1] in d[name2].parent:
            print (name1 + ' is a parent of ' + name2)
        elif d[name1] in d[name2].child:
            print (name1 + ' is a child of ' + name2)
        elif d[name1] in d[name2].sibling:
            print (name1 + ' is a sibling of ' + name2)
        elif d[name1] in d[name2].halfsibling:
            print (name1 + ' is a half-sibling of ' + name2)
        elif d[name1] in d[name2].ancestor:
            print (name1 + ' is a ancestor of ' + name2)
        elif d[name1] in d[name2].descendant:
            print (name1 + ' is a descendant of ' + name2)
        elif d[name1] in d[name2].cousin:
            print (name1 + ' is a cousin of ' + name2)
            
        else:
            print (name1 + ' and ' + name2 + ' are '
               'unrelated')

    if line_split[0] is 'X':
        name1 = line_split[1]
        relation = line_split[2]
        name2 = line_split[3]

        print ('\nX ' + name1 + ' ' + relation + ' ' + name2)

        if (len(name1) > 19 or '-' in name1 or
            len(name2) > 19 or '-' in name2):
            print (ncMsg)

        elif name1 not in d:
            print ('No')

        elif name2 not in d:
            print ('No')

        elif name1 == name2:
            print ('A person has no relation to his '
                   'or herself in this program.')

        elif relation == 'spouse':
            if d[name1] in d[name2].spouse:
                print ('Yes')
            else:
                print ('No')
                
        elif relation == 'parent':
            if d[name1] in d[name2].parent:
                print ('Yes')
            else:
                print ('No')

        elif relation == 'child':
            if d[name1] in d[name2].child:
                print ('Yes')
            else:
                print ('No')

        elif relation == 'sibling':
            if d[name1] in d[name2].sibling:
                print ('Yes')
            else:
                print ('No')

        elif relation == 'half-sibling':
            if d[name1] in d[name2].halfsibling:
                print ('Yes')
            else:
                print ('No')

        elif relation == 'ancestor':
            if d[name1] in d[name2].ancestor:
                print ('Yes')
            else:
                print ('No')

        elif relation == 'descendant':
            if d[name1] in d[name2].descendant:
                print ('Yes')
            else:
                print ('No')

        elif relation == 'cousin':
            if d[name1] in d[name2].cousin:
                print ('Yes')
            else:
                print ('No')
        else:
            print ('No')

                
    if line_split[0] is 'W':

        nameConstraints = False
        relation = line_split[1]
        name1 = line_split[2]

        print ('\nW ' + relation + ' ' + name1)

        if len(name1) > 19 or '-' in name1:
            print (ncMsg)

        else:
            if name1 in d:
                
                g = [] # list to be sorted alphabetically
                
                # keyword 'is' does not work here. Must use '=='
                if relation == 'spouse':
                    if d[name1].spouse:
                        print ('List of ' + name1 + '\'s spouses:')
                        for listItr in range(len(d[name1].spouse)):
                            g.append(d[name1].spouse[listItr].toStr())
                        g.sort()
                        for listItr in range(len(g)):
                            print (g[listItr])
                    else:
                        print (name1 + ' has no spouses')


                if relation == 'parent':
                    if d[name1].parent:
                        print ('List of ' + name1 + '\'s parents:')
                        for listItr in range(len(d[name1].parent)):
                            g.append(d[name1].parent[listItr].toStr())
                        g.sort()
                        for listItr in range(len(g)):
                            print (g[listItr])
                    else:
                        print (name1 + ' has no parents')

                if relation == 'child':
                    if d[name1].child:  # if not empty
                        print ('List of ' + name1 + '\'s children:')
                        for listItr in range(len(d[name1].child)):
                            g.append(d[name1].child[listItr].toStr())
                        g.sort()
                        for listItr in range(len(g)):
                            print (g[listItr])
                    else:
                        print (name1 + ' has no children')


                if relation == 'sibling':
                    if d[name1].sibling:
                        print ('List of ' + name1 + '\'s siblings:')
                        for listItr in range(len(d[name1].sibling)):
                            g.append(d[name1].sibling[listItr].toStr())
                        g.sort()
                        for listItr in range(len(g)):
                            print (g[listItr])
                    else:
                        print (name1 + ' has no siblings')
                        
                if relation == 'half-sibling':
                    if d[name1].halfsibling:
                        print ('List of ' + name1 + '\'s half-siblings:')
                        for listItr in range(len(d[name1].halfsibling)):
                            g.append(d[name1].halfsibling[listItr].toStr())
                        g.sort()
                        for listItr in range(len(g)):
                            print (g[listItr])
                    else:
                        print (name1 + ' has no half-siblings')

                if relation == 'ancestor':
                    if d[name1].ancestor:
                        print ('List of ' + name1 + '\'s ancestors:')
                        for listItr in range(len(d[name1].ancestor)):
                            g.append(d[name1].ancestor[listItr].toStr())
                        g.sort()
                        for listItr in range(len(g)):
                            print (g[listItr])
                    else:
                        print (name1 + ' has no ancestors')

                if relation == 'descendant':
                    if d[name1].descendant:
                        print ('List of ' + name1 + '\'s descendants:')
                        for listItr in range(len(d[name1].descendant)):
                            g.append(d[name1].descendant[listItr].toStr())
                        g.sort()
                        for listItr in range(len(g)):
                            print (g[listItr])
                    else:
                        print (name1 + ' has no descendants')
                    
                if relation == 'cousin':
                    if d[name1].cousin:
                        print ('List of ' + name1 + '\'s cousins:')
                        for listItr in range(len(d[name1].cousin)):
                            g.append(d[name1].cousin[listItr].toStr())
                        g.sort()
                        for listItr in range(len(g)):
                            print (g[listItr])
                    else:
                        print (name1 + ' has no cousins')
                    
            else: # if name1 not in d
                print (name1 + ' has no relatives')