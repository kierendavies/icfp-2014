# multiAgents.py
# --------------
# Licensing Information: Please do not distribute or publish solutions to this
# project. You are free to use and extend these projects for educational
# purposes. The Pacman AI projects were developed at UC Berkeley, primarily by
# John DeNero (denero@cs.berkeley.edu) and Dan Klein (klein@cs.berkeley.edu).
# For more info, see http://inst.eecs.berkeley.edu/~cs188/sp09/pacman.html

from util import manhattanDistance
from game import Directions
import random, util

from game import Agent

class ReflexAgent(Agent):
  """
    A reflex agent chooses an action at each choice point by examining
    its alternatives via a state evaluation function.

    The code below is provided as a guide.  You are welcome to change
    it in any way you see fit, so long as you don't touch our method
    headers.
  """


  def getAction(self, gameState):
    """
    You do not need to change this method, but you're welcome to.

    getAction chooses among the best options according to the evaluation function.

    Just like in the previous project, getAction takes a GameState and returns
    some Directions.X for some X in the set {North, South, West, East, Stop}
    """
    # Collect legal moves and successor states
    legalMoves = gameState.getLegalActions()

    # Choose one of the best actions
    scores = [self.evaluationFunction(gameState, action) for action in legalMoves]
    bestScore = max(scores)
    bestIndices = [index for index in range(len(scores)) if scores[index] == bestScore]
    chosenIndex = random.choice(bestIndices) # Pick randomly among the best

    "Add more of your code here if you want to"

    return legalMoves[chosenIndex]

  def evaluationFunction(self, currentGameState, action):
    """
    Design a better evaluation function here.

    The evaluation function takes in the current and proposed successor
    GameStates (pacman.py) and returns a number, where higher numbers are better.

    The code below extracts some useful information from the state, like the
    remaining food (newFood) and Pacman position after moving (newPos).
    newScaredTimes holds the number of moves that each ghost will remain
    scared because of Pacman having eaten a power pellet.

    Print out these variables to see what you're getting, then combine them
    to create a masterful evaluation function.
    """
    # Useful information you can extract from a GameState (pacman.py)
    successorGameState = currentGameState.generatePacmanSuccessor(action)
    newPos = successorGameState.getPacmanPosition()
    newFood = successorGameState.getFood()
    newGhostStates = successorGameState.getGhostStates()
    newScaredTimes = [ghostState.scaredTimer for ghostState in newGhostStates]

    "*** YOUR CODE HERE ***"
    #move onto block that is next to a ghost = 0
    #move towards nothing = 1
    #move towards food = 2
    score = 10
    closestGhost = 10000
    for ngs in newGhostStates:
      dist = manhattanDistance(newPos,ngs.getPosition())
      #print 'ngs.getPosition(): ',ngs.getPosition()
      #print 'dist: ',dist
      if (dist < 3):
        if (dist < closestGhost):
          closestGhost = dist
        
        score = closestGhost

    start = newPos
    foodPositions = newFood.asList()
    if (score == 10):
      if (action == 'Stop'):
        score = 0
      else:
        """
        if (len(newFood.asList()) == len(currentGameState.getFood().asList())):
          score = 10
        else:
          score = 20
        """
        foodDistances = []
        if len(foodPositions) > 0:
          for i in range(0, len(foodPositions)):
            dist = util.manhattanDistance(start,foodPositions[i])
            foodDistances.append((i, dist))
          foodDistances = sorted(foodDistances, key=lambda tup: tup[1])

          closestFood = foodDistances[0]
          score += (1/float(closestFood[1]+1))*100
        score += (1/float(len(foodPositions)+1))*10000
        score += 1
        """
        """
    

    #print 'score: ',score
    #print 'action: ',action
    #print 'foodPositions: ',foodPositions

    #return successorGameState.getScore()
    return score

def scoreEvaluationFunction(currentGameState):
  """
    This default evaluation function just returns the score of the state.
    The score is the same one displayed in the Pacman GUI.

    This evaluation function is meant for use with adversarial search agents
    (not reflex agents).
  """
  return currentGameState.getScore()

class MultiAgentSearchAgent(Agent):
  """
    This class provides some common elements to all of your
    multi-agent searchers.  Any methods defined here will be available
    to the MinimaxPacmanAgent, AlphaBetaPacmanAgent & ExpectimaxPacmanAgent.

    You *do not* need to make any changes here, but you can if you want to
    add functionality to all your adversarial search agents.  Please do not
    remove anything, however.

    Note: this is an abstract class: one that should not be instantiated.  It's
    only partially specified, and designed to be extended.  Agent (game.py)
    is another abstract class.
  """

  def __init__(self, evalFn = 'scoreEvaluationFunction', depth = '2'):
    self.index = 0 # Pacman is always agent index 0
    self.evaluationFunction = util.lookup(evalFn, globals())
    self.depth = int(depth)

class MinimaxAgent(MultiAgentSearchAgent):
  """
    Your minimax agent (question 2)
  """

  def getAction(self, gameState):
    """
      Returns the minimax action from the current gameState using self.depth
      and self.evaluationFunction.

      Here are some method calls that might be useful when implementing minimax.

      gameState.getLegalActions(agentIndex):
        Returns a list of legal actions for an agent
        agentIndex=0 means Pacman, ghosts are >= 1

      Directions.STOP:
        The stop direction, which is always legal

      gameState.generateSuccessor(agentIndex, action):
        Returns the successor game state after an agent takes an action

      gameState.getNumAgents():
        Returns the total number of agents in the game
    """
    "*** YOUR CODE HERE ***"

    #print (float("inf")*-1)
    depth = self.depth
    agentIndex = 0
    bestActionList = [Directions.STOP]
    bestVal = self.minimax(gameState, depth, agentIndex, bestActionList)
    print "bestVal:\t",bestVal,"bestAction:\t",bestActionList[0]
    return bestActionList[0]


  def minimax(self, gameState, depth, agentIndex, bestActionList):
    #maxDepth = 4
    #print "depth:\t",depth,"agentIndex:\t",agentIndex

    legalActions = gameState.getLegalActions(agentIndex)

    #if the state is a terminal state: return the state's utility
    if ((depth == 0) or (len(legalActions) == 0)):
      return self.evaluationFunction(gameState)

    
    nextAgentIndex = agentIndex + 1
    if (nextAgentIndex >= gameState.getNumAgents()):
      nextAgentIndex = 0
      depth -= 1

    
    v = 0
    currentBestAction = legalActions[0]
    if (agentIndex == 0):
      #if the next agent is MAX: return max-value(state)
      v = float("-inf")
      for action in legalActions:
        newGameSate = gameState.generateSuccessor(agentIndex, action)
        newV = max(v, self.minimax(newGameSate, depth, nextAgentIndex, bestActionList))
        if (newV != v):
          currentBestAction = action
        v = newV
      bestActionList[0] = currentBestAction
    else:
      #if the next agent is MIN: return min-value(state)
      v = float("inf")
      for action in legalActions:
        newGameSate = gameState.generateSuccessor(agentIndex, action)
        v = min(v, self.minimax(newGameSate, depth, nextAgentIndex, bestActionList))
    
    return v


class AlphaBetaAgent(MultiAgentSearchAgent):
  """
    Your minimax agent with alpha-beta pruning (question 3)
  """

  def getAction(self, gameState):
    """
      Returns the minimax action using self.depth and self.evaluationFunction
    """
    "*** YOUR CODE HERE ***"
    
    depth = self.depth
    agentIndex = 0
    bestActionList = [Directions.STOP]
    alpha = float("-inf")
    beta = float("inf")
    bestVal = self.minimaxAB(gameState, depth, alpha, beta, agentIndex, bestActionList)
    print "bestVal:\t",bestVal,"bestAction:\t",bestActionList[0]
    return bestActionList[0]

  def minimaxAB(self, gameState, depth, alpha, beta, agentIndex, bestActionList):
    #maxDepth = 4
    #print "depth:\t",depth,"agentIndex:\t",agentIndex

    legalActions = gameState.getLegalActions(agentIndex)

    #if the state is a terminal state: return the state's utility
    if ((depth == 0) or (len(legalActions) == 0)):
      return self.evaluationFunction(gameState)

    
    nextAgentIndex = agentIndex + 1
    if (nextAgentIndex >= gameState.getNumAgents()):
      nextAgentIndex = 0
      depth -= 1

    
    v = 0
    currentBestAction = legalActions[0]
    if (agentIndex == 0):
      #if the next agent is MAX: return max-value(state)
      v = float("-inf")
      for action in legalActions:
        newGameSate = gameState.generateSuccessor(agentIndex, action)
        newV = max(v, self.minimaxAB(newGameSate, depth, alpha, beta, nextAgentIndex, bestActionList))
        if (newV >= beta):   #beta cutoff
          v = newV
          break
        if (newV != v):
          currentBestAction = action
        v = newV
        alpha = max(alpha,v)
      bestActionList[0] = currentBestAction
    else:
      #if the next agent is MIN: return min-value(state)
      v = float("inf")
      for action in legalActions:
        newGameSate = gameState.generateSuccessor(agentIndex, action)
        v = min(v, self.minimaxAB(newGameSate, depth, alpha, beta, nextAgentIndex, bestActionList))
        if (v <= alpha):   #alpha cutoff
          break
        beta = min(beta,v)
    
    return v

class ExpectimaxAgent(MultiAgentSearchAgent):
  """
    Your expectimax agent (question 4)
  """

  def getAction(self, gameState):
    """
      Returns the expectimax action using self.depth and self.evaluationFunction

      All ghosts should be modeled as choosing uniformly at random from their
      legal moves.
    """
    "*** YOUR CODE HERE ***"
    
    
    depth = self.depth
    agentIndex = 0
    bestActionList = [Directions.STOP]
    bestVal = self.expmax(gameState, depth, agentIndex, bestActionList)
    print "bestVal:\t",bestVal,"bestAction:\t",bestActionList[0]
    return bestActionList[0]

  def expmax(self, gameState, depth, agentIndex, bestActionList):
    #maxDepth = 4
    #print "depth:\t",depth,"agentIndex:\t",agentIndex

    legalActions = gameState.getLegalActions(agentIndex)

    #if the state is a terminal state: return the state's utility
    if ((depth == 0) or (len(legalActions) == 0)):
      return self.evaluationFunction(gameState)

    
    nextAgentIndex = agentIndex + 1
    if (nextAgentIndex >= gameState.getNumAgents()):
      nextAgentIndex = 0
      depth -= 1

    
    v = 0
    currentBestAction = legalActions[0]
    if (agentIndex == 0):
      #if the next agent is MAX: return max-value(state)
      v = float("-inf")
      for action in legalActions:
        newGameSate = gameState.generateSuccessor(agentIndex, action)
        newV = max(v, self.expmax(newGameSate, depth, nextAgentIndex, bestActionList))
        if (newV != v):
          currentBestAction = action
        v = newV
      bestActionList[0] = currentBestAction
    else:
      v = 0.0
      for action in legalActions:
        newGameSate = gameState.generateSuccessor(agentIndex, action)
        v += self.expmax(newGameSate, depth, nextAgentIndex, bestActionList)
      v /= len(legalActions)
    
    return v

def betterEvaluationFunction(currentGameState):
  """
    Your extreme ghost-hunting, pellet-nabbing, food-gobbling, unstoppable
    evaluation function (question 5).

    DESCRIPTION: If the distance to the closest Ghost is less than three, that distance is the score.
                 Else the score is (1/(distance to closest food))*100 + (1/(number of food))*10000
                    +random [0,1] to prevent indecisiveness
  """
  "*** YOUR CODE HERE ***"
  #util.raiseNotDefined()
  #return 1000-len(currentGameState.getFood().asList())

  newPos = currentGameState.getPacmanPosition()
  newFood = currentGameState.getFood()
  newGhostStates = currentGameState.getGhostStates()

  score = 10
  closestGhost = 10000
  for ngs in newGhostStates:
    dist = manhattanDistance(newPos,ngs.getPosition())
    #print 'ngs.getPosition(): ',ngs.getPosition()
    #print 'dist: ',dist
    if (dist < 3):
      if (dist < closestGhost):
        closestGhost = dist
      
      score = closestGhost

  start = newPos
  foodPositions = newFood.asList()
  if (score == 10):
    """
    if (len(newFood.asList()) == len(currentGameState.getFood().asList())):
      score = 10
    else:
      score = 20
    """
    foodDistances = []
    if len(foodPositions) > 0:
      for i in range(0, len(foodPositions)):
        dist = util.manhattanDistance(start,foodPositions[i])
        foodDistances.append((i, dist))
      foodDistances = sorted(foodDistances, key=lambda tup: tup[1])

      closestFood = foodDistances[0]
      score += (1/float(closestFood[1]+1))*100
    score += (1/float(len(foodPositions)+len(currentGameState.getCapsules())+1))*10000
    score += (random.random()*0.1)
    score += 1
    """
    """
  return score

# Abbreviation
better = betterEvaluationFunction

class ContestAgent(MultiAgentSearchAgent):
  """
    Your agent for the mini-contest
  """

  def getAction(self, gameState):
    """
      Returns an action.  You can use any method you want and search to any depth you want.
      Just remember that the mini-contest is timed, so you have to trade off speed and computation.

      Ghosts don't behave randomly anymore, but they aren't perfect either -- they'll usually
      just make a beeline straight towards Pacman (or away from him if they're scared!)
    """
    "*** YOUR CODE HERE ***"
    util.raiseNotDefined()

