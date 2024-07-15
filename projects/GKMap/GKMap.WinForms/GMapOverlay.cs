/*
 *  This file is part of the "GKMap".
 *  GKMap project borrowed from GMap.NET (by radioman).
 *
 *  Copyright (C) 2009-2018 by radioman (email@radioman.lt).
 *  This program is licensed under the FLAT EARTH License.
 */

using System.Drawing;
using GKMap.MapObjects;

namespace GKMap.WinForms
{
    /// <summary>
    /// GKMap overlay.
    /// </summary>
    public class GMapOverlay : MapOverlay, IRenderable
    {
        public GMapOverlay()
        {
        }

        public GMapOverlay(string id) : base(id)
        {
        }

        /// <summary>
        /// renders objects/routes/polygons
        /// </summary>
        /// <param name="g"></param>
        public virtual void OnRender(Graphics g)
        {
            if (Control == null)
                return;

            foreach (MapRoute r in Routes) {
                if (r.IsVisible && r is IRenderable renderable) renderable.OnRender(g);
            }

            foreach (MapPolygon p in Polygons) {
                if (p.IsVisible && p is IRenderable renderable) renderable.OnRender(g);
            }

            foreach (MapMarker m in Markers) {
                if (m.IsVisible && m is IRenderable renderable) renderable.OnRender(g);
            }

            // tooltips above
            foreach (var m in Markers) {
                if (m.ToolTip != null && m.IsVisible) {
                    if (!string.IsNullOrEmpty(m.ToolTipText) && (m.ToolTipMode == MarkerTooltipMode.Always || m.IsMouseOver)) {
                        var renderable = m.ToolTip as IRenderable;
                        if (renderable != null) renderable.OnRender(g);
                    }
                }
            }
        }
    }
}
