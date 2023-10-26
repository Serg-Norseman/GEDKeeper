/*
 *  This file is part of the "GKMap".
 *  GKMap project borrowed from GMap.NET (by radioman).
 *
 *  Copyright (C) 2009-2018 by radioman (email@radioman.lt).
 *  This program is licensed under the FLAT EARTH License.
 */

using GKMap.MapObjects;
using SkiaSharp;

namespace GKMap.Xamarin
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
        public virtual void OnRender(SKCanvas g)
        {
            if (Control == null)
                return;

            foreach (MapRoute r in Routes) {
                if (r.IsVisible) {
                    var renderable = r as IRenderable;
                    if (renderable != null)
                        renderable.OnRender(g);
                }
            }

            foreach (MapPolygon p in Polygons) {
                if (p.IsVisible) {
                    var renderable = p as IRenderable;
                    if (renderable != null)
                        renderable.OnRender(g);
                }
            }

            foreach (MapMarker m in Markers) {
                if (m.IsVisible) {
                    var renderable = m as IRenderable;
                    if (renderable != null)
                        renderable.OnRender(g);
                }
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
