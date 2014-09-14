//const int inf=1000000; // inf > max capacity * E

// Input
int fV;
int G[MAX_V][MAX_V];
// Input: G[a][b] = capacity edge {a,b} (0 if no edge)
//        Do not forget to make it symetric if necessary
// Output: G[a][b] = residual capacity of {a, b}
//                 = initial capacity - flow

// Intermediates
int pi[MAX_V];
int CurrentNode[MAX_V];
int d[MAX_V];
int numbs[MAX_V+1];

void rev_BFS(int s, int t) {
  for(int i = 0; i < fV; i++)
    numbs[ d[i] = fV ] ++;

  numbs[fV]--;
  d[t] = 0;
  numbs[0]++;

  queue<int> q;
  q.push(t);

  while(!q.empty()) {
    int cur_v = q.front();
    q.pop();
    for(int v = 0; v < fV; v++) {
      if(d[v] < fV || G[v][cur_v] == 0)
        continue;
      q.push(v);
      numbs[fV]--;
      d[v] = d[cur_v] + 1;
      numbs[d[v]]++;
    }
  }
}

int Augment(int s, int t) {
  int width = inf;
  for(int cur = t, next = pi[t]; cur!=s; cur = next, next = pi[next])
    width = min(width, G[next][cur]);
  for(int cur = t, next = pi[t]; cur!=s; cur = next, next = pi[next]) {
    G[next][cur] -= width;
    G[cur][next] += width;
  }
  return width;
}

int Retreat(int &v, int s, int t) {
  int mind = fV-1;
  for(int next = 0; next < fV; next++)
    if(G[v][next] > 0)
      mind = min(mind, d[next]);
  int dv = d[v];
  numbs[dv]--;
  d[v] = 1 + mind;
  numbs[d[v]]++;
  if(v != s)
    v = pi[v];
  return numbs[dv];
}

int flow(int s, int t) {
  int res = 0;
  fill_n(numbs, fV+1, 0);
  rev_BFS(s, t);
  fill_n(CurrentNode, fV, 0);
  for(int cur_v = s; d[s] < fV;) {
    int v;
    for(v = CurrentNode[cur_v]; v < fV; v++)
      if(G[cur_v][v] > 0 && d[cur_v] == d[v] + 1)
        break;
    if(v < fV) {
      CurrentNode[cur_v] = v;
      pi[v] = cur_v;
      cur_v = v;
      if(cur_v == t) {
        res += Augment(s, t);
        cur_v = s;
      }
    } else {
      CurrentNode[cur_v] = 0;
      if(Retreat(cur_v, s, t) == 0)
        break;
    }
  }
  return res;
}
