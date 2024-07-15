/*
 * NetDiff (Diff4Net), v1.2.0.0
 * This is the C# implementation of the Diff algorithm.
 * Copyright © 2017 by skanmera
 * https://github.com/skanmera/NetDiff
 * License: MIT License (http://opensource.org/licenses/MIT)
 */

using System;
using System.Collections.Generic;
using System.Linq;

namespace GKCore.NetDiff
{
    public enum DiffStatus
    {
        Equal,
        Deleted,
        Inserted,
        Modified,
        DeepModified,
    }


    public class DiffResult<T>
    {
        public T Obj1 { get; private set; }
        public T Obj2 { get; private set; }
        public DiffStatus Status { get; set; }

        public DiffResult(T obj1, T obj2, DiffStatus status)
        {
            Obj1 = obj1;
            Obj2 = obj2;
            Status = status;
        }

        public override string ToString()
        {
            var obj = Status != DiffStatus.Inserted ? Obj1 : Obj2;
            return string.Format("{0} {1}", DiffUtil.GetStatusChar(Status), obj);
        }
    }


    public class DiffOption<T>
    {
        /// <summary>
        /// Specify IEqualityComparer to be used for comparing equality.
        /// </summary>
        public IEqualityComparer<T> EqualityComparer { get; set; }

        /// <summary>
        /// Specify the maximum number of nodes that can exist at once at the edit graph. 
        /// The lower the number, the better the performance, but the redundant differences increase.
        /// The default is 1000.
        /// </summary>
        public int Limit { get; set; }
    }


    public class DiffUtil
    {
        public static IEnumerable<DiffResult<T>> Diff<T>(IEnumerable<T> seq1, IEnumerable<T> seq2)
        {
            return Diff(seq1, seq2, new DiffOption<T>());
        }

        public static IEnumerable<DiffResult<T>> Diff<T>(IEnumerable<T> seq1, IEnumerable<T> seq2, DiffOption<T> option)
        {
            if (seq1 == null || seq2 == null || (!seq1.Any() && !seq2.Any()))
                return Enumerable.Empty<DiffResult<T>>();

            var editGrap = new EditGraph<T>(seq1, seq2);
            var waypoints = editGrap.CalculatePath(option);

            return MakeResults<T>(waypoints, seq1, seq2);
        }

        public static IEnumerable<T> CreateSrc<T>(IEnumerable<DiffResult<T>> diffResults)
        {
            return diffResults.Where(r => r.Status != DiffStatus.Inserted).Select(r => r.Obj1);
        }

        public static IEnumerable<T> CreateDst<T>(IEnumerable<DiffResult<T>> diffResults)
        {
            return diffResults.Where(r => r.Status != DiffStatus.Deleted).Select(r => r.Obj2);
        }

        public static IEnumerable<DiffResult<T>> OptimizeCaseDeletedFirst<T>(IEnumerable<DiffResult<T>> diffResults)
        {
            return Optimize(diffResults, true);
        }

        public static IEnumerable<DiffResult<T>> OptimizeCaseInsertedFirst<T>(IEnumerable<DiffResult<T>> diffResults)
        {
            return Optimize(diffResults, false);
        }

        private static IEnumerable<DiffResult<T>> Optimize<T>(IEnumerable<DiffResult<T>> diffResults, bool deleteFirst = true)
        {
            var currentStatus = deleteFirst ? DiffStatus.Deleted : DiffStatus.Inserted;
            var nextStatus = deleteFirst ? DiffStatus.Inserted : DiffStatus.Deleted;

            var queue = new Queue<DiffResult<T>>(diffResults);
            while (queue.Any())
            {
                var result = queue.Dequeue();
                if (result.Status == currentStatus)
                {
                    if (queue.Any() && queue.Peek().Status == nextStatus)
                    {
                        var obj1 = deleteFirst ? result.Obj1 : queue.Dequeue().Obj1;
                        var obj2 = deleteFirst ? queue.Dequeue().Obj2 : result.Obj2;
                        yield return new DiffResult<T>(obj1, obj2, DiffStatus.Modified);
                    }
                    else
                        yield return result;

                    continue;
                }

                yield return result;
            }
        }

        private static IEnumerable<DiffResult<T>> MakeResults<T>(IEnumerable<Point> waypoints, IEnumerable<T> seq1, IEnumerable<T> seq2)
        {
            var array1 = seq1.ToArray();
            var array2 = seq2.ToArray();

            foreach (var pair in MakePairsWithNext(waypoints))
            {
                var status = GetStatus(pair.Item1, pair.Item2);
                T obj1 = default;
                T obj2 = default;
                switch (status)
                {
                    case DiffStatus.Equal:
                        obj1 = array1[pair.Item2.X - 1];
                        obj2 = array2[pair.Item2.Y - 1];
                        break;
                    case DiffStatus.Inserted:
                        obj2 = array2[pair.Item2.Y - 1];
                        break;
                    case DiffStatus.Deleted:
                        obj1 = array1[pair.Item2.X - 1];
                        break;
                }

                yield return new DiffResult<T>(obj1, obj2, status);
            }
        }

        private static IEnumerable<Tuple<TSource, TSource>> MakePairsWithNext<TSource>(IEnumerable<TSource> source)
        {
            using (var enumerator = source.GetEnumerator())
            {
                if (enumerator.MoveNext())
                {
                    var previous = enumerator.Current;
                    while (enumerator.MoveNext())
                    {
                        var current = enumerator.Current;

                        yield return new Tuple<TSource, TSource>(previous, current);

                        previous = current;
                    }
                }
            }
        }

        private static DiffStatus GetStatus(Point current, Point prev)
        {
            if (current.X != prev.X && current.Y != prev.Y)
                return DiffStatus.Equal;
            else if (current.X != prev.X)
                return DiffStatus.Deleted;
            else if (current.Y != prev.Y)
                return DiffStatus.Inserted;
            else
                throw new Exception();
        }

        public static char GetStatusChar(DiffStatus status)
        {
            switch (status)
            {
                case DiffStatus.Equal:
                    return '=';
                case DiffStatus.Deleted:
                    return '-';
                case DiffStatus.Inserted:
                    return '+';
                case DiffStatus.Modified:
                case DiffStatus.DeepModified:
                    return '≠';
            }

            throw new System.Exception();
        }

        #region EditGraph

        internal enum Direction
        {
            Right,
            Bottom,
            Diagonal,
        }

        internal struct Point : IEquatable<Point>
        {
            public int X { get; private set; }
            public int Y { get; private set; }

            public Point(int x, int y) : this()
            {
                X = x;
                Y = y;
            }

            public override bool Equals(object obj)
            {
                if (!(obj is Point))
                    return false;

                return Equals((Point)obj);
            }

            public override int GetHashCode()
            {
                var hash = 17;
                hash = hash * 23 + X.GetHashCode();
                hash = hash * 23 + Y.GetHashCode();

                return hash;
            }

            public bool Equals(Point other)
            {
                return X == other.X && Y == other.Y;
            }

            public override string ToString()
            {
                return string.Format("X:{0} Y:{1}", X, Y);
            }
        }

        internal class Node
        {
            public Point Point { get; set; }
            public Node Parent { get; set; }

            public Node(Point point)
            {
                Point = point;
            }

            public override string ToString()
            {
                return string.Format("X:{0} Y:{1}", Point.X, Point.Y);
            }
        }

        internal class EditGraph<T>
        {
            private T[] seq1;
            private T[] seq2;
            private DiffOption<T> option;
            private List<Node> heads;
            private Point endpoint;
            private int[] farthestPoints;
            private int offset;
            private bool isEnd;

            public EditGraph(IEnumerable<T> seq1, IEnumerable<T> seq2)
            {
                this.seq1 = seq1.ToArray();
                this.seq2 = seq2.ToArray();
                endpoint = new Point(this.seq1.Length, this.seq2.Length);
                offset = this.seq2.Length;
            }

            public List<Point> CalculatePath(DiffOption<T> option)
            {
                if (!seq1.Any())
                    return Enumerable.Range(0, seq2.Length + 1).Select(i => new Point(0, i)).ToList();

                if (!seq2.Any())
                    return Enumerable.Range(0, seq1.Length + 1).Select(i => new Point(i, 0)).ToList();

                this.option = option;

                BeginCalculatePath();

                while (Next()) { }

                return EndCalculatePath();
            }

            private void Initialize()
            {
                farthestPoints = new int[seq1.Length + seq2.Length + 1];
                heads = new List<Node>();
            }

            private void BeginCalculatePath()
            {
                Initialize();

                heads.Add(new Node(new Point(0, 0)));

                Snake();
            }

            private List<Point> EndCalculatePath()
            {
                var wayponit = new List<Point>();

                var current = heads.Where(h => h.Point.Equals(endpoint)).FirstOrDefault();
                while (current != null)
                {
                    wayponit.Add(current.Point);

                    current = current.Parent;
                }

                wayponit.Reverse();

                return wayponit;
            }

            private bool Next()
            {
                if (isEnd)
                    return false;

                UpdateHeads();

                return true;
            }

            private void UpdateHeads()
            {
                if (option.Limit > 0 && heads.Count > option.Limit)
                {
                    var tmp = heads.First();
                    heads.Clear();

                    heads.Add(tmp);
                }

                var updated = new List<Node>();

                foreach (var head in heads)
                {
                    Node rightHead;
                    if (TryCreateHead(head, Direction.Right, out rightHead))
                    {
                        updated.Add(rightHead);
                    }

                    Node bottomHead;
                    if (TryCreateHead(head, Direction.Bottom, out bottomHead))
                    {
                        updated.Add(bottomHead);
                    }
                }

                heads = updated;

                Snake();
            }

            private void Snake()
            {
                var tmp = new List<Node>();
                foreach (var h in heads)
                {
                    var newHead = Snake(h);

                    if (newHead != null)
                        tmp.Add(newHead);
                    else
                        tmp.Add(h);
                }

                heads = tmp;
            }

            private Node Snake(Node head)
            {
                Node newHead = null;
                while (true)
                {
                    Node tmp;
                    if (TryCreateHead(newHead ?? head, Direction.Diagonal, out tmp))
                        newHead = tmp;
                    else
                        break;
                }

                return newHead;
            }

            private bool TryCreateHead(Node head, Direction direction, out Node newHead)
            {
                newHead = null;
                var newPoint = GetPoint(head.Point, direction);

                if (!CanCreateHead(head.Point, direction, newPoint))
                    return false;

                newHead = new Node(newPoint);
                newHead.Parent = head;

                isEnd |= newHead.Point.Equals(endpoint);

                return true;
            }

            private bool CanCreateHead(Point currentPoint, Direction direction, Point nextPoint)
            {
                if (!InRange(nextPoint))
                    return false;

                if (direction == Direction.Diagonal)
                {
                    var equal = option.EqualityComparer != null
                        ? option.EqualityComparer.Equals(seq1[nextPoint.X - 1], (seq2[nextPoint.Y - 1]))
                        : seq1[nextPoint.X - 1].Equals(seq2[nextPoint.Y - 1]);

                    if (!equal)
                        return false;
                }

                return UpdateFarthestPoint(nextPoint);
            }

            private Point GetPoint(Point currentPoint, Direction direction)
            {
                switch (direction)
                {
                    case Direction.Right:
                        return new Point(currentPoint.X + 1, currentPoint.Y);
                    case Direction.Bottom:
                        return new Point(currentPoint.X, currentPoint.Y + 1);
                    case Direction.Diagonal:
                        return new Point(currentPoint.X + 1, currentPoint.Y + 1);
                }

                throw new ArgumentException();
            }

            private bool InRange(Point point)
            {
                return point.X >= 0 && point.Y >= 0 && point.X <= endpoint.X && point.Y <= endpoint.Y;
            }

            private bool UpdateFarthestPoint(Point point)
            {
                var k = point.X - point.Y;
                var y = farthestPoints[k + offset];

                if (point.Y <= y)
                    return false;

                farthestPoints[k + offset] = point.Y;

                return true;
            }
        }

        #endregion
    }
}
