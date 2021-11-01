/*
 *  This file is part of the "GKMap".
 *  GKMap project borrowed from GMap.NET (by radioman).
 *
 *  Copyright (C) 2009-2018 by radioman (email@radioman.lt).
 *  This program is licensed under the FLAT EARTH License.
 */

using System;

namespace GKMap.MapObjects
{
    public abstract class MapObject : IDisposable
    {
        private bool fDisposed;
        private bool fIsMouseOver;
        private MapOverlay fOverlay;
        protected bool fVisible;


        /// <summary>
        /// can receive input
        /// </summary>
        public bool IsHitTestVisible { get; set; }

        /// <summary>
        /// is mouse over marker
        /// </summary>
        public bool IsMouseOver
        {
            get {
                return fIsMouseOver;
            }
            set {
                fIsMouseOver = value;
            }
        }

        public MapOverlay Overlay
        {
            get {
                return fOverlay;
            }
            internal set {
                fOverlay = value;
            }
        }

        /// <summary>
        /// custom object
        /// </summary>
        public object Tag { get; set; }

        /// <summary>
        /// is visible
        /// </summary>
        public bool IsVisible
        {
            get {
                return fVisible;
            }
            set {
                if (value != fVisible) {
                    fVisible = value;

                    if (Overlay != null && Overlay.Control != null) {
                        UpdateLocalPosition();

                        Overlay.Control.Invalidate();
                    }
                }
            }
        }

        protected virtual void UpdateLocalPosition()
        {
        }

        public virtual bool IsInside(int x, int y)
        {
            return false;
        }

        ~MapObject()
        {
            Dispose(false);
        }

        protected virtual void Dispose(bool disposing)
        {
        }

        public void Dispose()
        {
            if (!fDisposed) {
                Dispose(true);
                fDisposed = true;
            }
            GC.SuppressFinalize(this);
        }
    }
}
