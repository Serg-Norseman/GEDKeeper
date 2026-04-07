/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
 */

using GKCore.Charts;
using GKCore.Design.Graphics;
using GKUI.Platform.Handlers;
using Terminal.Gui;
using Attribute = Terminal.Gui.Attribute;

namespace GKUI.Platform
{
    /// <summary>
    /// 
    /// </summary>
    public sealed class TGGfxRenderer : TextRenderer
    {
        private View fTargetView;

        public TGGfxRenderer()
        {
        }

        public override void SetTarget(object target)
        {
            fTargetView = target as View;
            base.SetTarget(target);
        }

        protected override void DrawChar(int x, int y, char chr)
        {
            fTargetView?.AddRune(x, y, chr);
        }

        public override void DrawString(string text, IFont font, IBrush brush, float x, float y)
        {
            var driver = Application.Driver;
            var textAttr = new Attribute(Color.Black, Color.Gray);
            driver.SetAttribute(textAttr);

            base.DrawString(text, font, brush, x, y);
        }

        public override void DrawLine(IPen pen, float x1, float y1, float x2, float y2)
        {
            var penColor = (pen == null) ? null : ((PenHandler)pen).Color;
            var color = (penColor == null) ? Color.White : ((ColorHandler)penColor).Handle;

            var driver = Application.Driver;
            var lineAttr = new Attribute(color, Color.Blue);
            driver.SetAttribute(lineAttr);

            base.DrawLine(pen, x1, y1, x2, y2);
        }

        public override void DrawRectangle(IPen pen, IColor fillColor, float x, float y, float width, float height, int cornersRadius = 0)
        {
            var backColor = (fillColor == null) ? Color.Gray : ((ColorHandler)fillColor).Handle;

            var penColor = (pen == null) ? null : ((PenHandler)pen).Color;
            var foreColor = (penColor == null) ? Color.Black : ((ColorHandler)penColor).Handle;

            var driver = Application.Driver;
            var rectColor = new Attribute(foreColor, backColor);
            driver.SetAttribute(rectColor);

            base.DrawRectangle(pen, fillColor, x, y, width, height, cornersRadius);
        }

        public override void FillRectangle(IBrush brush, float x, float y, float width, float height)
        {
            base.FillRectangle(brush, x, y, width, height);
        }

        public override IPen CreatePen(IColor color, float width, float[] dashPattern = null)
        {
            return new PenHandler(color, width);
        }

        public override IBrush CreateBrush(IColor color)
        {
            return new BrushHandler(color);
        }
    }
}
