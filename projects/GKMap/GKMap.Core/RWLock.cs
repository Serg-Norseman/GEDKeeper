/*
 *  This file is part of the "GKMap".
 *  GKMap project borrowed from GMap.NET (by radioman).
 *
 *  Copyright (C) 2009-2018 by radioman (email@radioman.lt).
 *  This program is licensed under the FLAT EARTH License.
 */

using System;
using System.Threading;

namespace GKMap
{
    /// <summary>
    /// custom ReaderWriterLock
    /// </summary>
    public sealed class RWLock : IDisposable
    {
        private int fBusy;
        private int fReadCount;

        public void AcquireReaderLock()
        {
            Thread.BeginCriticalRegion();

            while (Interlocked.CompareExchange(ref fBusy, 1, 0) != 0) {
                Thread.Sleep(1);
            }

            Interlocked.Increment(ref fReadCount);

            // somehow this fix deadlock on heavy reads
            Thread.Sleep(0);
            Thread.Sleep(0);
            Thread.Sleep(0);
            Thread.Sleep(0);
            Thread.Sleep(0);
            Thread.Sleep(0);
            Thread.Sleep(0);

            Interlocked.Exchange(ref fBusy, 0);
        }

        public void ReleaseReaderLock()
        {
            Interlocked.Decrement(ref fReadCount);
            Thread.EndCriticalRegion();
        }

        public void AcquireWriterLock()
        {
            Thread.BeginCriticalRegion();

            while (Interlocked.CompareExchange(ref fBusy, 1, 0) != 0) {
                Thread.Sleep(1);
            }

            while (Interlocked.CompareExchange(ref fReadCount, 0, 0) != 0) {
                Thread.Sleep(1);
            }
        }

        public void ReleaseWriterLock()
        {
            Interlocked.Exchange(ref fBusy, 0);
            Thread.EndCriticalRegion();
        }

        public void Dispose()
        {
        }
    }
}
