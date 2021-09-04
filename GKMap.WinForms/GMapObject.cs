using System;
using System.Drawing;

namespace GKMap.WinForms
{
    public abstract class GMapObject : IDisposable
    {
        private bool fDisposed;
        private bool fIsMouseOver;
        private GMapOverlay fOverlay;
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
            internal set {
                fIsMouseOver = value;
            }
        }

        public GMapOverlay Overlay
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

                        if (!Overlay.Control.HoldInvalidation) {
                            Overlay.Control.Invalidate();
                        }
                    }
                }
            }
        }


        public virtual void OnRender(Graphics g)
        {
            //
        }

        protected virtual void UpdateLocalPosition()
        {
        }

        ~GMapObject()
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
