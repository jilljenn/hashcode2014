
const int MAX_L = 20000;
const int MAX_R = 20000;
// Computes a bipartite matching with minimum cost.

int L, R; // input: size of the sides of the bipartite graph
int cost[MAX_L][MAX_R]; // input: the graph,
// set cost[l][r] = infty if edge l->r doesn't exist

bool hadj[MAX_L][MAX_R];
bool reachedL[MAX_L], reachedR[MAX_R];
bool matchedL[MAX_L], visitedL[MAX_L];
// output: matchR : r in R is matched with matchR[r] in L
int matchR[MAX_R], slackL[MAX_L], slackR[MAX_R];

bool explore(int l) {
  reachedL[l] = 1;
  visitedL[l] = 1;
  for(int r = 0; r < R; r++)
    if(hadj[l][r]) {
      reachedR[r] = 1;
      if(matchR[r] < 0 || (!visitedL[matchR[r]] && explore(matchR[r]))) {
        matchR[r] = l;
        return 1;
      }
    }
  return 0;
}

bool bm() {
  fill_n(visitedL, L, 0);
  for(int l = 0; l < L; l++)
    if(!matchedL[l] && explore(l)) {
      fill_n(visitedL, L, 0);
      matchedL[l] = 1;
    }
  for(int l = 0; l < L; l++)
    if(!matchedL[l])
      return 0;
  return 1;
}

// Returns the weight of the match
int weighted_bm() {
  fill_n(matchR, R, -1);
  fill_n(matchedL, L, 0);
  fill_n(*hadj, MAX_R*MAX_L, 0);
  fill_n(slackR, R, 0);
  
  for(int l = 0; l < L; l++) {
    int rmin = 0;
    for(int r = 1; r < R; r++)
      if(cost[l][rmin] > cost[l][r])
        rmin = r;
    slackL[l] = cost[l][rmin];
    hadj[l][rmin] = 1;
  }
  
  for(;;) {
    fill_n(reachedL, L, 0);
    fill_n(reachedR, R, 0);
    
    if(bm())
      break;
    
    int m = infty;
    for(int l = 0; l < L; l++)
      for(int r = 0; r < R; r++)
        if(reachedL[l] && !reachedR[r] && !hadj[l][r])
          m = min(m, cost[l][r] - slackL[l] - slackR[r]);
    
    for(int l = 0; l < L; l++)
      if(reachedL[l])
        slackL[l] += m;
    for(int r = 0; r < R; r++)
      if(reachedR[r])
        slackR[r] -= m;
    
    for(int l = 0; l < L; l++)
      for(int r = 0; r < R; r++)
        if((reachedL[l] && !reachedR[r]) || (!reachedL[l] && reachedR[r]))
          hadj[l][r] = slackL[l] + slackR[r] == cost[l][r];
  }
  
  int sum = 0;
  for(int r = 0; r < R; r++)
    if(matchR[r] >= 0)
      sum += cost[matchR[r]][r];
  return sum;
}
