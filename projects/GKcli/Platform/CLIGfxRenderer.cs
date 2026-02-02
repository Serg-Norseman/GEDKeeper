/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
 */

using BSLib;
using GKCore.Charts;
using GKCore.Design.Graphics;

namespace GKUI.Platform;

public sealed class CLIGfxRenderer : ChartRenderer
{

    public CLIGfxRenderer()
    {
    }

    public override void SetTarget(object target)
    {
    }

    public override void SetSmoothing(bool value)
    {
    }

    public override void DrawArc(IPen pen, float x, float y, float width, float height, float startAngle, float sweepAngle)
    {
        // implementation is impossible
    }

    public override void DrawImage(IImage image, float x, float y, float width, float height, string imName)
    {
        // implementation is impossible
    }

    public override void DrawImage(IImage image, ExtRect destinationRect, ExtRect sourceRect)
    {
        // implementation is impossible
    }

    public override ExtSizeF GetTextSize(string text, IFont font)
    {
        if (string.IsNullOrEmpty(text) || font == null)
            return ExtSizeF.Empty;

        return new ExtSizeF(text.Length, 1);
    }

    public override void DrawString(string text, IFont font, IBrush brush, float x, float y)
    {
        // *
    }

    public override void DrawLine(IPen pen, float x1, float y1, float x2, float y2)
    {
        // *
    }

    public override void DrawRectangle(IPen pen, IColor fillColor, float x, float y, float width, float height, int cornersRadius = 0)
    {
        // *
    }

    public override void FillRectangle(IBrush brush, float x, float y, float width, float height)
    {
        // *
    }

    public override void DrawPath(IPen pen, IBrush brush, IGfxPath path)
    {
        // implementation is impossible
    }

    public override IPen CreatePen(IColor color, float width, float[] dashPattern = null)
    {
        // *
        return null;
    }

    public override IBrush CreateBrush(IColor color)
    {
        // *
        return null;
    }

    public override IGfxPath CreateCirclePath(float x, float y, float width, float height)
    {
        // implementation is impossible
        return null;
    }

    public override IGfxPath CreateCircleSegmentPath(int ctX, int ctY, float inRad, float extRad, float wedgeAngle, float ang1, float ang2)
    {
        // implementation is impossible
        return null;
    }

    public override void SetTranslucent(float value)
    {
        // implementation is impossible
    }

    public override void ScaleTransform(float sx, float sy)
    {
        // implementation is impossible
    }

    public override void TranslateTransform(float dx, float dy)
    {
        // implementation is impossible
    }

    public override void RotateTransform(float angle)
    {
        // implementation is impossible
    }

    public override void RestoreTransform()
    {
        // implementation is impossible
    }

    public override void SaveTransform()
    {
        // implementation is impossible
    }
}
