#if !NET6_0_OR_GREATER

namespace System.Collections.Generic
{
    // Licensed to the .NET Foundation under one or more agreements.
    // The .NET Foundation licenses this file to you under the MIT license.

    using System;
    using System.Collections.Generic;
    using System.Diagnostics;
    using System.Runtime.CompilerServices;

    /// <summary>
    ///     Represents a min priority queue.
    /// </summary>
    /// <typeparam name="TElement">Specifies the type of elements in the queue.</typeparam>
    /// <typeparam name="TPriority">Specifies the type of priority associated with enqueued elements.</typeparam>
    /// <remarks>
    ///     Implements an array-backed quaternary min-heap. Each element is enqueued with an associated priority
    ///     that determines the dequeue order: elements with the lowest priority get dequeued first.
    /// </remarks>
    [DebuggerDisplay("Count = {Count}")]
    public class PriorityQueue<TElement, TPriority>
    {
        internal const int MaxLength = int.MaxValue;
        internal const string InvalidOperation_EmptyQueue = "The queue is empty.";

        /// <summary>
        ///     Specifies the arity of the d-ary heap, which here is quaternary.
        ///     It is assumed that this value is a power of 2.
        /// </summary>
        private const int Arity = 4;

        /// <summary>
        ///     The binary logarithm of <see cref="Arity" />.
        /// </summary>
        private const int Log2Arity = 2;

        /// <summary>
        ///     Represents an implicit heap-ordered complete d-ary tree, stored as an array.
        /// </summary>
        private (TElement Element, TPriority Priority)[] _nodes;

        /// <summary>
        ///     Custom comparer used to order the heap.
        /// </summary>
        private readonly IComparer<TPriority> _comparer;

        /// <summary>
        ///     The number of nodes in the heap.
        /// </summary>
        private int _size;

        /// <summary>
        ///     Version updated on mutation to help validate enumerators operate on a consistent state.
        /// </summary>
        private int _version;

        /// <summary>
        ///     Gets the number of elements contained in the <see cref="PriorityQueue{TElement, TPriority}" />.
        /// </summary>
        public int Count => _size;

        /// <summary>
        ///     Initializes a new instance of the <see cref="PriorityQueue{TElement, TPriority}" /> class.
        /// </summary>
        public PriorityQueue()
        {
            _nodes = Array.Empty<(TElement, TPriority)>();
            _comparer = InitializeComparer(null);
        }

        /// <summary>
        ///     Adds the specified element with associated priority to the <see cref="PriorityQueue{TElement, TPriority}" />.
        /// </summary>
        /// <param name="element">The element to add to the <see cref="PriorityQueue{TElement, TPriority}" />.</param>
        /// <param name="priority">The priority with which to associate the new element.</param>
        public void Enqueue(TElement element, TPriority priority)
        {
            // Virtually add the node at the end of the underlying array.
            // Note that the node being enqueued does not need to be physically placed
            // there at this point, as such an assignment would be redundant.

            int currentSize = _size++;
            _version++;

            if (_nodes.Length == currentSize) {
                Grow(currentSize + 1);
            }

            if (_comparer == null) {
                MoveUpDefaultComparer((element, priority), currentSize);
            } else {
                MoveUpCustomComparer((element, priority), currentSize);
            }
        }

        /// <summary>
        ///     Returns the minimal element from the <see cref="PriorityQueue{TElement, TPriority}" /> without removing it.
        /// </summary>
        /// <exception cref="InvalidOperationException">The <see cref="PriorityQueue{TElement, TPriority}" /> is empty.</exception>
        /// <returns>The minimal element of the <see cref="PriorityQueue{TElement, TPriority}" />.</returns>
        public TElement Peek()
        {
            if (_size == 0) {
                throw new InvalidOperationException(InvalidOperation_EmptyQueue);
            }

            return _nodes[0].Element;
        }

        /// <summary>
        ///     Removes and returns the minimal element from the <see cref="PriorityQueue{TElement, TPriority}" />.
        /// </summary>
        /// <exception cref="InvalidOperationException">The queue is empty.</exception>
        /// <returns>The minimal element of the <see cref="PriorityQueue{TElement, TPriority}" />.</returns>
        public TElement Dequeue()
        {
            if (_size == 0) {
                throw new InvalidOperationException(InvalidOperation_EmptyQueue);
            }

            TElement element = _nodes[0].Element;
            RemoveRootNode();
            return element;
        }

        /// <summary>
        ///     Removes all items from the <see cref="PriorityQueue{TElement, TPriority}" />.
        /// </summary>
        public void Clear()
        {
            // Clear the elements so that the gc can reclaim the references
            Array.Clear(_nodes, 0, _size);

            _size = 0;
            _version++;
        }

        /// <summary>
        ///     Sets the capacity to the actual number of items in the <see cref="PriorityQueue{TElement, TPriority}" />,
        ///     if that is less than 90 percent of current capacity.
        /// </summary>
        /// <remarks>
        ///     This method can be used to minimize a collection's memory overhead
        ///     if no new elements will be added to the collection.
        /// </remarks>
        public void TrimExcess()
        {
            int threshold = (int)(_nodes.Length * 0.9);
            if (_size < threshold) {
                Array.Resize(ref _nodes, _size);
                _version++;
            }
        }

        /// <summary>
        ///     Grows the priority queue to match the specified min capacity.
        /// </summary>
        private void Grow(int minCapacity)
        {
            Debug.Assert(_nodes.Length < minCapacity);

            const int GrowFactor = 2;
            const int MinimumGrow = 4;

            int newcapacity = GrowFactor * _nodes.Length;

            // Allow the queue to grow to maximum possible capacity (~2G elements) before encountering overflow.
            // Note that this check works even when _nodes.Length overflowed thanks to the (uint) cast
            if ((uint)newcapacity > MaxLength) {
                newcapacity = MaxLength;
            }

            // Ensure minimum growth is respected.
            newcapacity = Math.Max(newcapacity, _nodes.Length + MinimumGrow);

            // If the computed capacity is still less than specified, set to the original argument.
            // Capacities exceeding Array.MaxLength will be surfaced as OutOfMemoryException by Array.Resize.
            if (newcapacity < minCapacity) {
                newcapacity = minCapacity;
            }

            Array.Resize(ref _nodes, newcapacity);
        }

        /// <summary>
        ///     Removes the node from the root of the heap
        /// </summary>
        private void RemoveRootNode()
        {
            int lastNodeIndex = --_size;
            _version++;

            if (lastNodeIndex > 0) {
                (TElement Element, TPriority Priority) lastNode = _nodes[lastNodeIndex];
                if (_comparer == null) {
                    MoveDownDefaultComparer(lastNode, 0);
                } else {
                    MoveDownCustomComparer(lastNode, 0);
                }
            }

            _nodes[lastNodeIndex] = default;
        }

        /// <summary>
        ///     Gets the index of an element's parent.
        /// </summary>
        private static int GetParentIndex(int index) => index - 1 >> Log2Arity;

        /// <summary>
        ///     Gets the index of the first child of an element.
        /// </summary>
        private static int GetFirstChildIndex(int index) => (index << Log2Arity) + 1;

        /// <summary>
        ///     Moves a node up in the tree to restore heap order.
        /// </summary>
        private void MoveUpDefaultComparer((TElement Element, TPriority Priority) node, int nodeIndex)
        {
            // Instead of swapping items all the way to the root, we will perform
            // a similar optimization as in the insertion sort.

            Debug.Assert(_comparer is null);
            Debug.Assert(0 <= nodeIndex && nodeIndex < _size);

            (TElement Element, TPriority Priority)[] nodes = _nodes;

            while (nodeIndex > 0) {
                int parentIndex = GetParentIndex(nodeIndex);
                (TElement Element, TPriority Priority) parent = nodes[parentIndex];

                if (Comparer<TPriority>.Default.Compare(node.Priority, parent.Priority) < 0) {
                    nodes[nodeIndex] = parent;
                    nodeIndex = parentIndex;
                } else {
                    break;
                }
            }

            nodes[nodeIndex] = node;
        }

        /// <summary>
        ///     Moves a node up in the tree to restore heap order.
        /// </summary>
        private void MoveUpCustomComparer((TElement Element, TPriority Priority) node, int nodeIndex)
        {
            // Instead of swapping items all the way to the root, we will perform
            // a similar optimization as in the insertion sort.

            IComparer<TPriority> comparer = _comparer;
            (TElement Element, TPriority Priority)[] nodes = _nodes;

            while (nodeIndex > 0) {
                int parentIndex = GetParentIndex(nodeIndex);
                (TElement Element, TPriority Priority) parent = nodes[parentIndex];

                if (comparer.Compare(node.Priority, parent.Priority) < 0) {
                    nodes[nodeIndex] = parent;
                    nodeIndex = parentIndex;
                } else {
                    break;
                }
            }

            nodes[nodeIndex] = node;
        }

        /// <summary>
        ///     Moves a node down in the tree to restore heap order.
        /// </summary>
        private void MoveDownDefaultComparer((TElement Element, TPriority Priority) node, int nodeIndex)
        {
            // The node to move down will not actually be swapped every time.
            // Rather, values on the affected path will be moved up, thus leaving a free spot
            // for this value to drop in. Similar optimization as in the insertion sort.

            Debug.Assert(_comparer is null);
            Debug.Assert(0 <= nodeIndex && nodeIndex < _size);

            (TElement Element, TPriority Priority)[] nodes = _nodes;
            int size = _size;

            int i;
            while ((i = GetFirstChildIndex(nodeIndex)) < size) {
                // Find the child node with the minimal priority
                (TElement Element, TPriority Priority) minChild = nodes[i];
                int minChildIndex = i;

                int childIndexUpperBound = Math.Min(i + Arity, size);
                while (++i < childIndexUpperBound) {
                    (TElement Element, TPriority Priority) nextChild = nodes[i];
                    if (Comparer<TPriority>.Default.Compare(nextChild.Priority, minChild.Priority) < 0) {
                        minChild = nextChild;
                        minChildIndex = i;
                    }
                }

                // Heap property is satisfied; insert node in this location.
                if (Comparer<TPriority>.Default.Compare(node.Priority, minChild.Priority) <= 0) {
                    break;
                }

                // Move the minimal child up by one node and
                // continue recursively from its location.
                nodes[nodeIndex] = minChild;
                nodeIndex = minChildIndex;
            }

            nodes[nodeIndex] = node;
        }

        /// <summary>
        ///     Moves a node down in the tree to restore heap order.
        /// </summary>
        private void MoveDownCustomComparer((TElement Element, TPriority Priority) node, int nodeIndex)
        {
            // The node to move down will not actually be swapped every time.
            // Rather, values on the affected path will be moved up, thus leaving a free spot
            // for this value to drop in. Similar optimization as in the insertion sort.

            IComparer<TPriority> comparer = _comparer;
            (TElement Element, TPriority Priority)[] nodes = _nodes;
            int size = _size;

            int i;
            while ((i = GetFirstChildIndex(nodeIndex)) < size) {
                // Find the child node with the minimal priority
                (TElement Element, TPriority Priority) minChild = nodes[i];
                int minChildIndex = i;

                int childIndexUpperBound = Math.Min(i + Arity, size);
                while (++i < childIndexUpperBound) {
                    (TElement Element, TPriority Priority) nextChild = nodes[i];
                    if (comparer.Compare(nextChild.Priority, minChild.Priority) < 0) {
                        minChild = nextChild;
                        minChildIndex = i;
                    }
                }

                // Heap property is satisfied; insert node in this location.
                if (comparer.Compare(node.Priority, minChild.Priority) <= 0) {
                    break;
                }

                // Move the minimal child up by one node and continue recursively from its location.
                nodes[nodeIndex] = minChild;
                nodeIndex = minChildIndex;
            }

            nodes[nodeIndex] = node;
        }

        /// <summary>
        ///     Initializes the custom comparer to be used internally by the heap.
        /// </summary>
        private static IComparer<TPriority> InitializeComparer(IComparer<TPriority> comparer)
        {
            if (typeof(TPriority).IsValueType) {
                if (comparer == Comparer<TPriority>.Default) {
                    // if the user manually specifies the default comparer,
                    // revert to using the optimized path.
                    return null;
                }

                return comparer;
            } else {
                // Currently the JIT doesn't optimize direct Comparer<T>.Default.Compare
                // calls for reference types, so we want to cache the comparer instance instead.
                // TODO https://github.com/dotnet/runtime/issues/10050: Update if this changes in the future.
                return comparer ?? Comparer<TPriority>.Default;
            }
        }
    }
}

#endif
