/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
 */

using System;
using System.Collections.Generic;
using System.Linq;
using BSLib;

namespace GKCore.Charts
{
    public class LineRouter
    {
        private readonly List<TreeChartPerson> fPersons;
        private readonly List<LineHandle> fLines;
        private List<int> fCoordsX;
        private List<int> fCoordsY;

        public LineRouter(List<TreeChartPerson> persons, List<LineHandle> lines)
        {
            fCoordsX = new List<int>();
            fCoordsY = new List<int>();

            fPersons = persons;
            fLines = lines;
        }

        public void Route(TreeChartPerson start, TreeChartPerson end, List<LineHandle> output)
        {
            var endpoints1 = GetNodeEndpoints(start.Rect);
            var endpoints2 = GetNodeEndpoints(end.Rect);

            ExtPoint bestPt1, bestPt2;
            FindClosestPair(endpoints1, endpoints2, out bestPt1, out bestPt2);

            Route(bestPt1, bestPt2, output);
        }

        public void Route(ExtPoint start, ExtPoint end, List<LineHandle> output)
        {
            BuildAdaptiveGrid(start, end, 10);

            var reconstructPath = FindPath(start, end);
            var finalPath = LineRouter.SimplifyPath(reconstructPath);
            if (finalPath == null || finalPath.Count == 0) return;

            var prevPt = finalPath[0];
            for (int i = 1; i < finalPath.Count; i++) {
                var curr = finalPath[i];
                output.Add(new LineHandle(prevPt.X, prevPt.Y, curr.X, curr.Y));
                prevPt = curr;
            }
        }

        public List<ExtPoint> GetNodeEndpoints(ExtRect rect)
        {
            var result = new List<ExtPoint>();

            var wp = rect.Width / 4;
            var w1 = rect.Left + wp;
            var w2 = rect.Left + wp * 3;

            result.Add(new ExtPoint(w1, rect.Top - 5));
            result.Add(new ExtPoint(w2, rect.Top - 5));

            result.Add(new ExtPoint(w1, rect.Bottom + 5));
            result.Add(new ExtPoint(w2, rect.Bottom + 5));

            /*var hp = rect.Height / 4;
            var h1 = rect.Top + hp;
            var h2 = rect.Top + hp * 3;

            result.Add(new ExtPoint(rect.Left - 5, h1));
            result.Add(new ExtPoint(rect.Left - 5, h2));

            result.Add(new ExtPoint(rect.Right + 5, h1));
            result.Add(new ExtPoint(rect.Right + 5, h2));*/

            return result;
        }

        private static void FindClosestPair(List<ExtPoint> list1, List<ExtPoint> list2, out ExtPoint bestPt1, out ExtPoint bestPt2)
        {
            bestPt1 = list1[0];
            bestPt2 = list2[0];
            double minDistance = double.MaxValue;

            foreach (var p1 in list1) {
                foreach (var p2 in list2) {
                    double distance = ManhattanDistance(p1, p2);
                    if (distance < minDistance) {
                        minDistance = distance;
                        bestPt1 = p1;
                        bestPt2 = p2;
                    }
                }
            }
        }

        private void BuildAdaptiveGrid(ExtPoint start, ExtPoint end, int padding)
        {
            var xSet = new HashSet<int> { start.X, end.X };
            var ySet = new HashSet<int> { start.Y, end.Y };

            foreach (var pers in fPersons) {
                var rect = pers.Rect;
                var hh = rect.Top + rect.Height / 2;
                var wp = rect.Width / 3;
                var w1 = rect.Left + wp;
                var w2 = rect.Left + wp * 2;

                xSet.Add(rect.Left - padding);
                xSet.Add(w1);
                xSet.Add(w2);
                xSet.Add(rect.Right + padding);

                ySet.Add(rect.Top - padding);
                ySet.Add(hh);
                ySet.Add(rect.Bottom + padding);
            }

            fCoordsX = xSet.OrderBy(v => v).ToList();
            fCoordsY = ySet.OrderBy(v => v).ToList();
        }

        private IEnumerable<ExtPoint> GetNeighbors(ExtPoint current)
        {
            // We find the indices of the current point in our lists of axes
            int xi = fCoordsX.BinarySearch(current.X);
            int yi = fCoordsY.BinarySearch(current.Y);

            // If the point is not on the grid (for example, start/finish),
            // BinarySearch will return a negative number. This needs to be handled.
            if (xi < 0 || yi < 0) yield break;

            // Neighbor on the Right
            if (xi < fCoordsX.Count - 1)
                yield return new ExtPoint(fCoordsX[xi + 1], fCoordsY[yi]);

            // Neighbor on the Left
            if (xi > 0)
                yield return new ExtPoint(fCoordsX[xi - 1], fCoordsY[yi]);

            // Neighbor on the Top
            if (yi > 0)
                yield return new ExtPoint(fCoordsX[xi], fCoordsY[yi - 1]);

            // Neighbor on the Bottom
            if (yi < fCoordsY.Count - 1)
                yield return new ExtPoint(fCoordsX[xi], fCoordsY[yi + 1]);
        }

        private class Node
        {
            public ExtPoint Position;
            public Vector Direction; // The direction in which we arrived at the node
            public float G;          // The real cost of the path from the start
            public float H;          // Heuristics (Manhattan to the Finish)
            public float F => G + H;
            public Node Parent;
        }

        private List<ExtPoint> FindPath(ExtPoint start, ExtPoint end)
        {
            var openSet = new PriorityQueue<Node, float>();
            // Dictionary for storing the best cost to visit a point with a specific destination
            // (direction is important because turning changes the future cost)
            var closedSet = new Dictionary<(ExtPoint, Vector), float>();

            Node startNode = new Node { Position = start, Direction = Vector.Zero, G = 0, H = LineRouter.ManhattanDistance(start, end) };
            openSet.Enqueue(startNode, startNode.F);

            while (openSet.Count > 0) {
                var current = openSet.Dequeue();

                if (current.Position.Equals(end))
                    return ReconstructPath(current);

                var key = (current.Position, current.Direction);
                if (closedSet.TryGetValue(key, out float bestG) && bestG <= current.G) continue;
                closedSet[key] = current.G;

                var neighbors = GetNeighbors(current.Position);
                foreach (var neighborPos in neighbors) {
                    // Checking for blocks
                    bool anyBlock = fPersons.Any(pers => IsLineIntersectingRect(current.Position, neighborPos, pers.Rect));
                    if (anyBlock) continue;

                    Vector newDir = GetDirection(current.Position, neighborPos);

                    // Calculation of weights
                    float stepDist = ManhattanDistance(current.Position, neighborPos);
                    float turnPenalty = (current.Direction != Vector.Zero && current.Direction != newDir) ? 100f : 0f;
                    float linePenalty = CountIntersections(current.Position, neighborPos) * 500f;

                    float tentativeG = current.G + stepDist + turnPenalty + linePenalty;

                    Node neighborNode = new Node {
                        Position = neighborPos,
                        Direction = newDir,
                        G = tentativeG,
                        H = ManhattanDistance(neighborPos, end),
                        Parent = current
                    };

                    openSet.Enqueue(neighborNode, neighborNode.F);
                }
            }
            return null; // Path not found
        }

        private static Vector GetDirection(ExtPoint from, ExtPoint to)
        {
            return new Vector(Math.Sign(to.X - from.X), Math.Sign(to.Y - from.Y));
        }

        private readonly struct Vector
        {
            public static Vector Zero = new Vector(0, 0);

            public readonly int X;
            public readonly int Y;

            public Vector(int x, int y)
            {
                X = x;
                Y = y;
            }

            public static bool operator ==(Vector a, Vector b) => a.X == b.X && a.Y == b.Y;
            public static bool operator !=(Vector a, Vector b) => !(a == b);
        }

        private List<ExtPoint> ReconstructPath(Node endNode)
        {
            var path = new List<ExtPoint>();

            var current = endNode;
            while (current != null) {
                path.Add(current.Position);
                current = current.Parent;
            }

            // We flip it so that the path is from Start to End
            path.Reverse();

            return path;
        }

        private int CountIntersections(ExtPoint p1, ExtPoint p2)
        {
            int count = 0;
            var lines = fLines;
            foreach (var line in lines) {
                var xp1 = new ExtPoint(line.X1, line.Y1);
                var xp2 = new ExtPoint(line.X2, line.Y2);
                if (DoSegmentsIntersect(p1, p2, xp1, xp2))
                    count++;
            }
            return count;
        }

        #region Utilities

        public static int ManhattanDistance(ExtPoint p1, ExtPoint p2)
        {
            return Math.Abs(p1.X - p2.X) + Math.Abs(p1.Y - p2.Y);
        }

        /// <summary>
        /// Liang–Barsky line clipping algorithm.
        /// </summary>
        public static bool IsLineIntersectingRect(ExtPoint a, ExtPoint b, ExtRect rect)
        {
            float x1 = a.X, y1 = a.Y, x2 = b.X, y2 = b.Y;
            float minX = rect.Left, minY = rect.Top, maxX = rect.Right, maxY = rect.Bottom;

            float t0 = 0.0f;
            float t1 = 1.0f;
            float dx = x2 - x1;
            float dy = y2 - y1;

            float[] p = { -dx, dx, -dy, dy };
            float[] q = { x1 - minX, maxX - x1, y1 - minY, maxY - y1 };

            for (int i = 0; i < 4; i++) {
                if (p[i] == 0) {
                    if (q[i] < 0) return false; // The segment is parallel and outside
                } else {
                    float t = q[i] / p[i];
                    if (p[i] < 0) {
                        if (t > t1) return false;
                        if (t > t0) t0 = t;
                    } else {
                        if (t < t0) return false;
                        if (t < t1) t1 = t;
                    }
                }
            }
            return t0 <= t1;
        }

        /// <summary>
        /// Simplifying and smoothing the path - removing unnecessary points that lie along a straight line.
        /// </summary>
        public static List<ExtPoint> SimplifyPath(List<ExtPoint> path)
        {
            if (path == null || path.Count <= 2) return path;

            var simplified = new List<ExtPoint> { path[0] };

            for (int i = 1; i < path.Count - 1; i++) {
                ExtPoint prev = path[i - 1];
                ExtPoint curr = path[i];
                ExtPoint next = path[i + 1];

                // We check collinearity for orthogonal lines:
                // If X is the same for all three or Y is the same for all three, it is a straight line.
                bool isHorizontal = Math.Abs(prev.Y - curr.Y) < 0.001f && Math.Abs(curr.Y - next.Y) < 0.001f;
                bool isVertical = Math.Abs(prev.X - curr.X) < 0.001f && Math.Abs(curr.X - next.X) < 0.001f;

                if (!isHorizontal && !isVertical) {
                    simplified.Add(curr);
                }
            }

            simplified.Add(path[path.Count - 1]);
            return simplified;
        }

        public static bool DoSegmentsIntersect(ExtPoint p1, ExtPoint p2, ExtPoint p3, ExtPoint p4)
        {
            // Function to find the orientation of a triple of points
            // 0 -> the points are collinear
            // 1 -> clockwise
            // 2 -> counterclockwise
            int GetOrientation(ExtPoint p, ExtPoint q, ExtPoint r)
            {
                float val = (q.Y - p.Y) * (r.X - q.X) - (q.X - p.X) * (r.Y - q.Y);
                if (Math.Abs(val) < 0.001f) return 0;
                return (val > 0) ? 1 : 2;
            }

            // Checking whether a point q lies on a segment pr
            bool OnSegment(ExtPoint p, ExtPoint q, ExtPoint r)
            {
                return q.X <= Math.Max(p.X, r.X) && q.X >= Math.Min(p.X, r.X) &&
                       q.Y <= Math.Max(p.Y, r.Y) && q.Y >= Math.Min(p.Y, r.Y);
            }

            int o1 = GetOrientation(p1, p2, p3);
            int o2 = GetOrientation(p1, p2, p4);
            int o3 = GetOrientation(p3, p4, p1);
            int o4 = GetOrientation(p3, p4, p2);

            // General case: orientations are different
            if (o1 != o2 && o3 != o4) return true;

            // Special cases (collinearity and being on an interval)
            if (o1 == 0 && OnSegment(p1, p3, p2)) return true;
            if (o2 == 0 && OnSegment(p1, p4, p2)) return true;
            if (o3 == 0 && OnSegment(p3, p1, p4)) return true;
            if (o4 == 0 && OnSegment(p3, p2, p4)) return true;

            return false;
        }

        #endregion
    }
}
