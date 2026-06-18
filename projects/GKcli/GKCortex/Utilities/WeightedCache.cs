/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
 */

using System;
using System.Collections.Generic;
using System.Threading;

namespace GKCortex.Utilities;

public class WeightedCache<TKey, TValue> : IDisposable where TKey : notnull
{
    private class Node : IComparable<Node>
    {
        public TKey Key { get; }
        public TValue Value { get; }
        public double Weight { get; set; }
        public long Timestamp { get; }

        public Node(TKey key, TValue value, double weight)
        {
            Key = key;
            Value = value;
            Weight = weight;
            Timestamp = DateTime.UtcNow.Ticks;
        }

        public int CompareTo(Node? other)
        {
            if (other == null) return 1;
            // Primary sort by weight, secondary by timestamp (FIFO for same weights)
            int res = Weight.CompareTo(other.Weight);
            return res != 0 ? res : Timestamp.CompareTo(other.Timestamp);
        }
    }

    private readonly int fMaxCapacity;
    private readonly Dictionary<TKey, Node> fDict = new();
    private readonly SortedSet<Node> fSortedNodes = new();
    private readonly ReaderWriterLockSlim fLock = new();

    private double fGlobalBaseWeight = 0;
    private const double DecayStep = 0.1;

    public int Count
    {
        get {
            fLock.EnterReadLock();
            try { return fDict.Count; } finally { fLock.ExitReadLock(); }
        }
    }

    public WeightedCache(int maxCapacity)
    {
        fMaxCapacity = maxCapacity;
    }

    public void Dispose()
    {
        fLock.Dispose();
    }

    public void Add(TKey key, TValue value)
    {
        fLock.EnterWriteLock();
        try {
            if (fDict.ContainsKey(key)) return;

            // Evict the "lightest" and oldest element if capacity is reached
            if (fDict.Count >= fMaxCapacity) {
                var minNode = fSortedNodes.Min;
                if (minNode != null) {
                    fDict.Remove(minNode.Key);
                    fSortedNodes.Remove(minNode);
                }
            }

            // New elements enter with the current inflation-adjusted weight
            var newNode = new Node(key, value, fGlobalBaseWeight);
            fDict.Add(key, newNode);
            fSortedNodes.Add(newNode);

            fGlobalBaseWeight += DecayStep;
        } finally {
            fLock.ExitWriteLock();
        }
    }

    public TValue Get(TKey key)
    {
        // Upgradeable read lock allows reading first, then switching to write if update is needed
        fLock.EnterUpgradeableReadLock();
        try {
            if (!fDict.TryGetValue(key, out var node)) return default;

            fLock.EnterWriteLock();
            try {
                // Update weight: Remove, change value, and Re-insert to maintain sorting
                fSortedNodes.Remove(node);
                node.Weight = fGlobalBaseWeight + 1.0;
                fSortedNodes.Add(node);

                fGlobalBaseWeight += DecayStep;
            } finally {
                fLock.ExitWriteLock();
            }

            return node.Value;
        } finally {
            fLock.ExitUpgradeableReadLock();
        }
    }

    public void Clear()
    {
        fLock.EnterWriteLock();
        try {
            fDict.Clear();
            fSortedNodes.Clear();
            fGlobalBaseWeight = 0;
        } finally {
            fLock.ExitWriteLock();
        }
    }
}
