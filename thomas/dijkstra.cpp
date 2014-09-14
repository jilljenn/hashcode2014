const int MAX_V = 20000;

// Input: 'adj', 'orig'
// Output: 'path', 'dist'
typedef pair<int, pii> pipii;

vector<vector<pii> > adj; // adj. list with pair(secondend, weight)
int V; // input
int path[MAX_V][MAX_V];
int dist[MAX_V][MAX_V];

const int inf = (2*1000*1000*1000); // > max length of a shortest path

void dijkstra(int orig) {
  fill_n(path[orig], V, -1);
  fill_n(dist[orig], V, inf);
  priority_queue<pipii, vector<pipii>, greater<pipii> > Q;
  Q.push(pipii(0, pii(orig, orig)));
  while(!Q.empty()) {
    int d = Q.top().first;
    int v1 = Q.top().second.first;
    int v2 = Q.top().second.second;
    Q.pop();
    if (path[orig][v2] >= 0)
      continue;
    path[orig][v2] = v1;
    dist[orig][v2] = d;
    for (int i = 0; i < adj[v2].size(); i++)
      Q.push(pipii(adj[v2][i].second+dist[orig][v2], pii(v2, adj[v2][i].first)));
  }
}
