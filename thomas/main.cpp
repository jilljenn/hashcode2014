#include <cstdlib>
#include "header.cpp"
#include "hungarian.cpp"
#include "dijkstra.cpp"
#include "flow_sap.cpp"
#include <assert.h>

int ncars;
int runtime;
int nverts;
int nedges;
int start;

double dgarbage;
int garbage;


int ine[MAX_V];
int oute[MAX_V];
int inoute[MAX_V];

vector<vector<pii> > oadj; // oriented adj. list
vector<pii> noadj; // adj. list of non-oriented vectors
vector<vector<int> > foadj;

bool augmark[MAX_V];

bool augrec(int v, int s) {
  if ( augmark[v] ) return false;
  else {
    augmark[v]=true;
    if ( ine[v]-oute[v] > s ) { ine[v]--; oute[v]++; return true; }
    for(int i = 0; i < foadj[v].size(); i++) {
      int nxt = foadj[v][i];
      if (augrec(nxt,s)) {
        foadj[v].erase(foadj[v].begin()+i);
        foadj[nxt].push_back(v);
        return true;
      }
    }
    return false;
  }
}

bool augment(int v, int s) {
  for ( int i = 0; i < MAX_V; i ++) augmark[i]=false;
  if ( augrec(v,s) ) { ine[v]++; oute[v]--; return true; }
  return false;
}

int main() {
  scanf("%d%d%d%d%d", &nverts, &nedges, &runtime, &ncars, &start);
  
  V = nverts;

  int totcost=0;

  srand(time(NULL));

  // The hard problem is orientation of the bilateral edges

  for(int i=0; i < nverts; i++){
    ine[i]=oute[i]=inoute[i]=0;
    scanf("%lf%lf", &dgarbage, &dgarbage);
    adj.push_back(vector<pii>());
    oadj.push_back(vector<pii>());
    foadj.push_back(vector<int>());
  }

  /* Edge allocation : we want to maximize the number of edges that
     need zero extras ? Or even minimize the sum of (squared ?) differences
     Greedy allocation : use the orientation that minimizes the differences
  */

  for(int i=0;i<nverts+2;i++)
    for(int j=0;j<nverts+2;j++)
      G[i][j]=0;

  // Read edges
  for(int i=0; i < nedges; i++) {
    int orig;
    int dest;
    int ow;
    int cost;
    scanf("%d%d%d%d%d", &orig, &dest, &ow, &cost, &garbage);
    
    totcost+=cost;
    
    adj[orig].push_back(pair<int,int>(dest,cost));
    // 0 = pure, 1 = path
     //    directdist[orig][dest]=cost;
    if ( ow == 1 ) {
      oute[orig]++;
      ine[dest]++;
      oadj[orig].push_back(pair<int,int>(dest,0));
    } else {
      adj[dest].push_back(pair<int,int>(orig, cost));
      G[orig][dest]=G[dest][orig]=1; // Assume unicity
      noadj.push_back(pair<int,int>(orig,dest));
    }
  }

  int src = nverts;
  int dst=nverts+1;
  for (int i=0;i<nverts;i++) {
    G[src][i]=max<int>(0,ine[i]-oute[i]);
    G[i][dst]=max<int>(0,oute[i]-ine[i]);
    assert(G[src][i] == 0 || G[i][dst]==0);
  }


  fV = V+2;  
  fprintf(stderr,"Flow: %d\n", flow(src,dst));

  int nthings=0;
  int ndoubles=0;
  int nnulls=0;
  while ( noadj.size() > 0 ) {
    nthings++;
    pii p = noadj.back();
    noadj.pop_back();
    if ( G[p.first][p.second] == 0 ) {
      if ( G[p.second][p.first] == 0)
        ndoubles++;
      else {
        oadj[p.first].push_back(pair<int,int>(p.second,0));
        oute[p.first]++; ine[p.second]++;
      }
    }
    else if ( G[p.second][p.first] == 0 ) {
      oadj[p.second].push_back(pair<int,int>(p.first,0));
      oute[p.second]++; ine[p.first]++;
    } else {
      if (rand()%2 == 0 ){
        int t = p.first;
        p.first = p.second;
        p.second = t;
      }
      foadj[p.first].push_back(p.second);
      // oadj[p.first].push_back(pair<int,int>(p.second,0));
      /* oute[p.first]++; ine[p.second]++; */
      inoute[p.first]++; inoute[p.second]++;
      oute[p.first]++; ine[p.second]++;
      nnulls++;
    }
  }
  fprintf(stderr,"Things : %d, Nulls : %d, Doubles : %d, Effective : %d\n", nthings, nnulls, ndoubles, nthings - nnulls - ndoubles);

  // we allow reversing things in foadj.
  bool did = true;
  while(did) {
    did=false;
    fprintf(stderr,"Iterating\n");
    for(int i=0; i < nverts; i++) {
      if ( oute[i] - ine[i] > 0 )
        if ( augment(i, (oute[i]-ine[i] == 1)?1:0) ) did = true;
    }
  }

  for(int i = 0; i < oadj[6619].size(); i++)
    assert(oadj[6619][i].first != 386);
  /*
  bool done;
  do {
    done = false;
    int scorechange = 0;
    for(int i=0;i<noadj.size();i++) {
      pii p = noadj[i];
      int x = p.first, y = p.second;
      if ( abs(ine[x]-oute[x]+2) + abs(ine[y]-oute[y]-2)
           < abs(ine[x]-oute[x]) + abs(ine[y] - oute[y])) {
        scorechange++;
        done=true;
        noadj[i]=pair<int,int>(y,x);
        oute[x]--; ine[x]++;
        oute[y]++; ine[y]--;
      }
    }
    fprintf(stderr,"Changed %d\n", scorechange);
  } while ( done );
  */

  /* Build the mapping : who is in what bucket ? */
  int thingsL[MAX_L];
  int nbthingsL=0;
  int thingsR[MAX_L];
  int nbthingsR=0;

  for(int i=0; i < nverts; i++) {
    // We need things that go there !
    if ( ine[i] /* + inoute[i] */ < oute[i] ) {
      for (int j=0; j < oute[i] - (ine[i] /* +inoute[i] */);j++) {
        thingsR[nbthingsR++]=i;
      }
    }
    else if ( oute[i] /* + inoute[i] */ < ine[i] ) {
      for (int j=0; j < ine[i] - (oute[i]/*+inoute[i]*/);j++) {
        thingsL[nbthingsL++]=i;
      }
    } /* else if ( (oute[i] + inoute[i] - ine[i]) % 2 != 0) {
      if (counter == 0) {
        counter=1;
        thingsL[nbthingsL++]=i;
      } else {
        counter = 0;
        thingsR[nbthingsR++]=i;
      }
      } */
  }

  L = nbthingsL;
  R = nbthingsR;
  fprintf(stderr, "Got L=%d R=%d\n", L, R);


  for(int i=0; i < nverts; i++) {
    dijkstra(i);
    assert(path[i][i] == i);
    assert(dist[i][i] == 0);
  }

  fprintf(stderr, "Done dijkstra\n");
  /* Ok Google, now initialize the distance matrix */
  for(int i=0;i<nbthingsL;i++)
    for(int j=0;j<nbthingsR;j++)
      cost[i][j]=dist[thingsL[i]][thingsR[j]];

  int r = weighted_bm();
  fprintf(stderr, "Extra cost : %d, total cost : %d, credit : %d\n", r, r + totcost, runtime * ncars);

  /* Compter le nombre d'utilisations de chaque vertex dans chaque sens :
     est-ce qu'on peut raccourcir un peu ? */
  /*  for(int i=0;i<nverts;i++)
    for(int j=0;i<nverts;i++)
      trips[i][j]=0;

  for(int i=0;i<oadj.size();i++) {
    trips[oadj[i].first][oadj[i].second]++;
    } */
  for(int i=0;i<nverts;i++)
    for(int j=0;j<foadj[i].size();j++) {
      oadj[i].push_back(pair<int,int>(foadj[i][j],0));
      //trips[i][foadj[i][j]]++;
    }
  for(int i = 0; i < oadj[6619].size(); i++)
    assert(oadj[6619][i].first != 386);

  /*
  for(int i=0;i<L;i++) {
    fprintf(stdout,"%d\n",cost[matchR[i]][i]);
    }*/

  for(int i=0; i<L; i++) {
    int l = matchR[i];
    // artificial !
    oadj[thingsL[matchR[i]]].push_back(pair<int,int>(thingsR[i],1));
    
  }
  for(int i = 0; i < oadj[6619].size(); i++)
    assert(oadj[6619][i].first != 386 || oadj[6619][i].second != 0);

  /* Compute the in and out degrees for each node */
  for(int i=0;i<nverts;i++)
    ine[i]=oute[i]=0;

  for(int i=0;i<nverts;i++)
    for(int j = 0; j < oadj[i].size(); j++) {
      oute[i]++;
      ine[oadj[i][j].first]++;
    }

  for(int i=0;i<nverts;i++)
    assert(ine[i] == oute[i]);
  // start with start, then at each step take the first edge that goes out
  // if there is no such edge, look in the matching
  // if we go back to the start, find another edge, do a thing
  // it is equivalent to adding all the edges given by the hungarian algorithm

  vector<pii> toconfirm;
  vector<pii> confirmed;

  toconfirm.push_back(pair<int,int>(start,0));
  
  // Start with the first "toconfirm" and explore until we are stuck
  // Then we will have to extend the paths !
  while(! toconfirm.empty() ){
    pii p = toconfirm.back();
    int v = p.first;
    if ( oadj[v].size() > 0 ) {
      int cur = v;
      while ( oadj[cur].size() > 0 ) {
        pii foo = oadj[cur].back();
        oadj[cur].pop_back();
        cur = foo.first;
        toconfirm.push_back(foo);
      }
      assert( cur == v);
    }
    //printf("Went from %d to %d\n", v, toconfirm.back().first);
    assert ( v == toconfirm.back().first );
    confirmed.push_back(toconfirm.back());
    toconfirm.pop_back(); // This is a v
  }
  /*
  for(int i=0;i<confirmed.size();i++)
    printf("%d (%d)\n", confirmed[i].first, confirmed[i].second);
  */
  fprintf(stderr,"Size of eulerian path : %d\n", confirmed.size());
  // Then we need to expand the paths...
  
  vector<int> fullpath;

  fullpath.push_back(start);
  while( confirmed.size() > 1) {
    pii x = confirmed[confirmed.size()-2];
    pii y = confirmed[confirmed.size()-1];
    int nx=x.first;
    confirmed.pop_back();
    vector<int> locpath;
    if ( x.second == 0 ) locpath.push_back(nx);
    else while (nx != y.first) {
        locpath.push_back(nx);
        nx=path[y.first][nx];
      }

    while(! locpath.empty() ){
      fullpath.push_back(locpath.back());
      locpath.pop_back();
    }
  }


  vector<vector<int> > edges;
  for(int i=0;i<nverts;i++) {
    oadj[i].clear();
    //edges.push_back(vector<int>());
    //dbedges.push_back(vector<int>());
  }


  for(int i=0;i<nverts;i++)
    for(int j=0;j<nverts;j++)
      cost[i][j]=0; // Reuse !

  // We could use the info here to generate hints about edge usage
  for(int i=0;i < fullpath.size()-1; i++) {
    int x = fullpath[i];
    int y = fullpath[i+1];
    //edges[x].push_back(y);
    cost[x][y]++;
    //printf("%d %d %s\n",min<int>(x,y), max<int>(x,y), (x<y)?"+":"-");
  }


  totcost = 0; 
  for(int i=0;i<nverts;i++) {
    for(int ij=0;ij<adj[i].size();ij++) {
      int j = adj[i][ij].first;
      int c = adj[i][ij].second;
      int n = (cost[i][j] > cost[j][i])?(cost[i][j]-cost[j][i]):(cost[i][j] == cost[j][i])?1:0;
      totcost += c*n;
      for(int k=0;k<n;k++)
        oadj[i].push_back(pair<int,int>(j,0));
    }
  }
  fprintf(stderr, "The cost : %d\n", totcost);

  /* Now remove some cycles/useless edges and do an eulerian path again */
  confirmed.clear();
  toconfirm.clear();
  fullpath.clear();

  toconfirm.push_back(pair<int,int>(start,0));
  
  // Start with the first "toconfirm" and explore until we are stuck
  // Then we will have to extend the paths !
  while(! toconfirm.empty() ){
    pii p = toconfirm.back();
    int v = p.first;
    if ( oadj[v].size() > 0 ) {
      int cur = v;
      while ( oadj[cur].size() > 0 ) {
        pii foo = oadj[cur].back();
        oadj[cur].pop_back();
        cur = foo.first;
        toconfirm.push_back(foo);
      }
      assert( cur == v);
    }
    //printf("Went from %d to %d\n", v, toconfirm.back().first);
    assert ( v == toconfirm.back().first );
    confirmed.push_back(toconfirm.back());
    toconfirm.pop_back(); // This is a v
  }

  fprintf(stderr,"Size of eulerian path : %d\n", confirmed.size());
  // Then we need to expand the paths...
  
  fullpath.push_back(start);
  while( confirmed.size() > 1) {
    pii x = confirmed[confirmed.size()-2];
    pii y = confirmed[confirmed.size()-1];
    int nx=x.first;
    confirmed.pop_back();
    vector<int> locpath;
    if ( x.second == 0 ) locpath.push_back(nx);
    else while (nx != y.first) {
        locpath.push_back(nx);
        nx=path[y.first][nx];
      }

    while(! locpath.empty() ){
      fullpath.push_back(locpath.back());
      locpath.pop_back();
    }
  }
  
  fprintf(stderr, "Length : %d\n", fullpath.size());

  // TODO : compute the length

  printf("8\n");
  for(int i = 0; i < 7; i++)
    printf("1\n4516\n");
  
  printf("%d\n", fullpath.size());
  for(int i=0; i < fullpath.size(); i++)
    printf("%d\n", fullpath[i]);

  return 0;

}
