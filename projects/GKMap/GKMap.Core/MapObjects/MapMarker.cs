/*
 *  This file is part of the "GKMap".
 *  GKMap project borrowed from GMap.NET (by radioman).
 *
 *  Copyright (C) 2009-2018 by radioman (email@radioman.lt).
 *  This program is licensed under the FLAT EARTH License.
 */

namespace GKMap.MapObjects
{
    /// <summary>
    /// mode of tooltip
    /// </summary>
    public enum MarkerTooltipMode
    {
        OnMouseOver,
        Always,
    }

    public enum GMarkerIconType
    {
        none = 0,
        arrow,
        blue,
        blue_small,
        blue_dot,
        brown_small,
        gray_small,
        green,
        green_small,
        green_dot,
        yellow,
        yellow_small,
        yellow_dot,
        lightblue,
        lightblue_dot,
        orange,
        orange_small,
        orange_dot,
        pink,
        pink_dot,
        purple,
        purple_small,
        purple_dot,
        red,
        red_small,
        red_dot,
        black_small,
        white_small,
    }

    /// <summary>
    /// GKMap marker.
    /// </summary>
    public abstract class MapMarker : MapObject
    {
        private GRect fArea;
        private GPoint fOffset;
        private PointLatLng fPosition;
        private string fToolTipText;

        /// <summary>
        /// marker position in local coordinates, internal only, do not set it manually
        /// </summary>
        public GPoint LocalPosition
        {
            get {
                return fArea.Location;
            }
            set {
                if (fArea.Location != value) {
                    fArea.Location = value;

                    if (Overlay != null && Overlay.Control != null) {
                        Overlay.Control.Invalidate();
                    }
                }
            }
        }

        public GPoint Offset
        {
            get {
                return fOffset;
            }
            set {
                if (fOffset != value) {
                    fOffset = value;

                    if (IsVisible) {
                        if (Overlay != null && Overlay.Control != null) {
                            Overlay.Control.Core.UpdateMarkerLocalPosition(this);
                        }
                    }
                }
            }
        }

        public PointLatLng Position
        {
            get {
                return fPosition;
            }
            set {
                if (fPosition != value) {
                    fPosition = value;

                    if (IsVisible) {
                        if (Overlay != null && Overlay.Control != null) {
                            Overlay.Control.Core.UpdateMarkerLocalPosition(this);
                        }
                    }
                }
            }
        }

        public GSize Size
        {
            get {
                return fArea.Size;
            }
            set {
                fArea.Size = value;
            }
        }

        public MapToolTip ToolTip { get; set; }

        public MarkerTooltipMode ToolTipMode { get; set; }

        /// <summary>
        /// ToolTip position in local coordinates
        /// </summary>
        public GPoint ToolTipPosition
        {
            get {
                GPoint ret = fArea.Location;
                ret.Offset(-Offset.X, -Offset.Y);
                return ret;
            }
        }

        public virtual string ToolTipText
        {
            get {
                return fToolTipText;
            }
            set {
                fToolTipText = value;
            }
        }


        protected MapMarker(PointLatLng pos)
        {
            Position = pos;
            fVisible = true;
            ToolTipMode = MarkerTooltipMode.OnMouseOver;
            IsHitTestVisible = true;
        }

        public override bool IsInside(int x, int y)
        {
            return fArea.Contains(x, y);
        }

        protected override void UpdateLocalPosition()
        {
            if (fVisible) {
                Overlay.Control.Core.UpdateMarkerLocalPosition(this);
            } else {
                if (Overlay.Control.IsMouseOverMarker) {
                    Overlay.Control.IsMouseOverMarker = false;
                    Overlay.Control.RestoreCursorOnLeave();
                }
            }
        }

        protected override void Dispose(bool disposing)
        {
            if (disposing) {
                if (ToolTip != null) {
                    fToolTipText = null;
                    ToolTip.Dispose();
                    ToolTip = null;
                }
            }
        }
    }


    public abstract class MapIconMarker : MapMarker
    {
        public GMarkerIconType Type { get; private set; }

        protected MapIconMarker(PointLatLng pos) : base(pos)
        {
        }

        public MapIconMarker(PointLatLng p, GMarkerIconType type)
        : base(p)
        {
            Type = type;

            if (type != GMarkerIconType.none) {
                LoadBitmap();
            }
        }

        protected abstract void LoadBitmap();
    }
}
