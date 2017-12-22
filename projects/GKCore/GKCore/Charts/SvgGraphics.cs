// praeclarum/CrossGraphics
// Copyright (c) 2010-2013 Frank A. Krueger
//
// Permission is hereby granted, free of charge, to any person obtaining a copy
// of this software and associated documentation files (the "Software"), to deal
// in the Software without restriction, including without limitation the rights
// to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
// copies of the Software, and to permit persons to whom the Software is
// furnished to do so, subject to the following conditions:
//
// The above copyright notice and this permission notice shall be included in
// all copies or substantial portions of the Software.
//
// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
// IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
// FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
// AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
// LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
// OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
// THE SOFTWARE.

using System;
using System.Collections.Generic;
using System.Globalization;
using System.IO;

using GKCommon;
using GKCore.Interfaces;

namespace GKCore.Charts
{
    public enum LineBreakMode
    {
        None,
        Clip,
        WordWrap,
    }

    public enum TextAlignment
    {
        Left,
        Center,
        Right,
        Justified
    }

    public class SvgGraphics
    {
        private class State
        {
            public ExtPointF Scale;
            public ExtPointF Translation;
            public ExtRectF ClippingRect;
        }

        private bool fInGroup = false;
        private bool fInPolyline = false;
        private string fLastColor = null;
        private string fLastColorOpacity = null;
        private IFont fLastFont = null;
        private bool fStartedPolyline = false;
        private State fState = new State();

        private readonly IFormatProvider fFmt;
        private readonly Stack<State> fStates;
        private readonly ExtRectF fViewBox;
        private readonly TextWriter fWriter;

        public bool IncludeXmlAndDoctype { get; set; }

        public SvgGraphics(TextWriter tw, ExtRectF viewBox)
        {
            fFmt = CultureInfo.InvariantCulture;
            fStates = new Stack<State>();
            fViewBox = viewBox;
            fWriter = tw;
            IncludeXmlAndDoctype = true;
            //SetColor(Color.Black);
            fStates.Push(fState);
        }

        public SvgGraphics(TextWriter tw, ExtRect viewBox)
            : this(tw, (ExtRectF)viewBox)
        {
        }

        public SvgGraphics(Stream s, ExtRectF viewBox)
            : this(new StreamWriter(s, System.Text.Encoding.UTF8), viewBox)
        {
        }

        void WriteLine(string s)
        {
            fWriter.WriteLine(s);
        }

        void WriteLine(string format, params object[] args)
        {
            WriteLine(string.Format(fFmt, format, args));
        }

        void Write(string s)
        {
            fWriter.Write(s);
        }

        void Write(string format, params object[] args)
        {
            Write(string.Format(fFmt, format, args));
        }

        public void BeginDrawing()
        {
            if (IncludeXmlAndDoctype) {
                WriteLine(@"<?xml version=""1.0""?>
<!DOCTYPE svg PUBLIC ""-//W3C//DTD SVG 1.1//EN"" ""http://www.w3.org/Graphics/SVG/1.1/DTD/svg11.dtd"">");
            }
            WriteLine(@"<svg viewBox=""{0} {1} {2} {3}"" preserveAspectRatio=""xMinYMin meet"" version=""1.1"" xmlns=""http://www.w3.org/2000/svg"">",
                fViewBox.Left, fViewBox.Top, fViewBox.GetWidth(), fViewBox.GetHeight());

            fInGroup = false;
        }

        public void BeginEntity(object entity)
        {
            if (fInGroup) {
                WriteLine("</g>");
            }
            var klass = (entity != null) ? entity.ToString() : "";
            WriteLine("<g class=\"{0}\">", klass);
            fInGroup = true;
        }

        public void Clear(IColor clearColor)
        {
            WriteLine("<g id=\"background\"><rect x=\"{0}\" y=\"{1}\" width=\"{2}\" height=\"{3}\" fill=\"{4}\" stroke=\"none\"/></g>",
                fViewBox.Left, fViewBox.Top, fViewBox.GetWidth(), fViewBox.GetHeight(), FormatColor(clearColor));
        }

        public void EndDrawing()
        {
            if (fInGroup) {
                WriteLine("</g>");
                fInGroup = false;
            }
            WriteLine("</svg>");
            fWriter.Flush();
        }

        public void SaveState()
        {
            var ns = new State() {
                Translation = fState.Translation,
            };
            fStates.Push(ns);
            fState = ns;
        }

        public void Scale(float sx, float sy)
        {
            fState.Scale.X *= sx;
            fState.Scale.Y *= sy;
        }

        public void Translate(float dx, float dy)
        {
            fState.Translation.X += dx;
            fState.Translation.Y += dy;
        }

        public void SetClippingRect(float x, float y, float width, float height)
        {
            fState.ClippingRect = ExtRectF.CreateBounds(x, y, width, height);
        }

        public void RestoreState()
        {
            if (fStates.Count > 1) {
                fState = fStates.Pop();
            }
        }

        public void SetFont(IFont f)
        {
            fLastFont = f;
        }

        static string FormatColor(IColor c)
        {
            return string.Format("#{0:X2}{1:X2}{2:X2}", c.GetR(), c.GetG(), c.GetB());
        }

        public void SetColor(IColor c)
        {
            fLastColor = FormatColor(c);
            fLastColorOpacity = string.Format(fFmt, "{0}", c.GetA() / 255.0);
        }

        /*public void FillPolygon(Polygon poly)
        {
            Write("<polygon fill=\"{0}\" fill-opacity=\"{1}\" stroke=\"none\" points=\"", _lastColor, _lastColorOpacity);
            foreach (var p in poly.Points) {
                Write("{0}", p.X);
                Write(",");
                Write("{0}", p.Y);
                Write(" ");
            }
            WriteLine("\" />");
        }

        public void DrawPolygon(Polygon poly, float w)
        {
            Write("<polygon stroke=\"{0}\" stroke-opacity=\"{1}\" stroke-width=\"{2}\" fill=\"none\" points=\"", _lastColor, _lastColorOpacity, w);
            foreach (var p in poly.Points) {
                Write("{0}", p.X);
                Write(",");
                Write("{0}", p.Y);
                Write(" ");
            }
            WriteLine("\" />");
        }*/

        public void FillOval(float x, float y, float width, float height)
        {
            var rx = width / 2;
            var ry = height / 2;
            var cx = x + rx;
            var cy = y + ry;
            WriteLine("<ellipse cx=\"{0}\" cy=\"{1}\" rx=\"{2}\" ry=\"{3}\" fill=\"{4}\" fill-opacity=\"{5}\" stroke=\"none\" />",
                cx, cy, rx, ry, fLastColor, fLastColorOpacity);
        }

        public void DrawOval(float x, float y, float width, float height, float w)
        {
            var rx = width / 2;
            var ry = height / 2;
            var cx = x + rx;
            var cy = y + ry;
            WriteLine("<ellipse cx=\"{0}\" cy=\"{1}\" rx=\"{2}\" ry=\"{3}\" stroke=\"{4}\" stroke-opacity=\"{5}\" stroke-width=\"{6}\" fill=\"none\" />",
                cx, cy, rx, ry, fLastColor, fLastColorOpacity, w);
        }

        public void FillArc(float cx, float cy, float radius, float startAngle, float endAngle)
        {
            WriteArc(cx, cy, radius, startAngle, endAngle, 0, "none", "0", fLastColor, fLastColorOpacity);
        }

        public void DrawArc(float cx, float cy, float radius, float startAngle, float endAngle, float w)
        {
            WriteArc(cx, cy, radius, startAngle, endAngle, w, fLastColor, fLastColorOpacity, "none", "0");
        }

        public void WriteArc(float cx, float cy, float radius, float startAngle, float endAngle, float w, string stroke, string strokeOp, string fill, string fillOp)
        {
            var sa = startAngle + Math.PI;
            var ea = endAngle + Math.PI;

            var sx = cx + radius * Math.Cos(sa);
            var sy = cy + radius * Math.Sin(sa);
            var ex = cx + radius * Math.Cos(ea);
            var ey = cy + radius * Math.Sin(ea);

            WriteLine("<path d=\"M {0} {1} A {2} {3} 0 0 1 {4} {5}\" stroke=\"{6}\" stroke-opacity=\"{7}\" stroke-width=\"{8}\" fill=\"{9}\" fill-opacity=\"{10}\" />",
                sx, sy,
                radius, radius,
                ex, ey,
                stroke, strokeOp, w, fill, fillOp);
        }

        public void FillRoundedRect(float x, float y, float width, float height, float radius)
        {
            WriteLine("<rect x=\"{0}\" y=\"{1}\" width=\"{2}\" height=\"{3}\" rx=\"{6}\" ry=\"{6}\" fill=\"{4}\" fill-opacity=\"{5}\" stroke=\"none\" />",
                x, y, width, height, fLastColor, fLastColorOpacity, radius);
        }

        public void DrawRoundedRect(float x, float y, float width, float height, float radius, float w)
        {
            WriteLine("<rect x=\"{0}\" y=\"{1}\" width=\"{2}\" height=\"{3}\" rx=\"{7}\" ry=\"{7}\" stroke=\"{4}\" stroke-opacity=\"{5}\" stroke-width=\"{6}\" fill=\"none\" />",
                x, y, width, height, fLastColor, fLastColorOpacity, w, radius);
        }

        public void FillRect(float x, float y, float width, float height)
        {
            WriteLine("<rect x=\"{0}\" y=\"{1}\" width=\"{2}\" height=\"{3}\" fill=\"{4}\" fill-opacity=\"{5}\" stroke=\"none\" />",
                x, y, width, height, fLastColor, fLastColorOpacity);
        }

        public void DrawRect(float x, float y, float width, float height, float w)
        {
            WriteLine("<rect x=\"{0}\" y=\"{1}\" width=\"{2}\" height=\"{3}\" stroke=\"{4}\" stroke-opacity=\"{5}\" stroke-width=\"{6}\" fill=\"none\" />",
                x, y, width, height, fLastColor, fLastColorOpacity, w);
        }

        public void BeginLines(bool rounded)
        {
            fInPolyline = true;
        }

        public void DrawLine(float sx, float sy, float ex, float ey, float w)
        {
            if (fInPolyline) {
                if (!fStartedPolyline) {
                    Write("<polyline stroke=\"{0}\" stroke-opacity=\"{1}\" stroke-linecap=\"round\" stroke-linejoin=\"round\" stroke-width=\"{2}\" fill=\"none\" points=\"", fLastColor, fLastColorOpacity, w);
                    Write("{0},{1} ", sx, sy);
                    fStartedPolyline = true;
                }
                Write("{0},{1} ", ex, ey);
            } else {
                WriteLine("<line x1=\"{0}\" y1=\"{1}\" x2=\"{2}\" y2=\"{3}\" stroke=\"{4}\" stroke-opacity=\"{5}\" stroke-width=\"{6}\" stroke-linecap=\"round\" fill=\"none\" />", sx, sy, ex, ey, fLastColor, fLastColorOpacity, w);
            }
        }

        public void EndLines()
        {
            if (fInPolyline) {
                WriteLine("\" />");
                fInPolyline = false;
                fStartedPolyline = false;
            }
        }

        public void DrawString(string s, float x, float y, float width, float height, LineBreakMode lineBreak, TextAlignment align)
        {
            WriteLine("<text x=\"{0}\" y=\"{1}\" font-family=\"{2}\" font-size=\"{3}\">{4}</text>",
                x, y + fLastFont.Size, fLastFont.FontFamilyName, 
                fLastFont.Size * 3 / 2,
                s.Replace("&", "&amp;").Replace("<", "&lt;").Replace(">", "&gt;"));
        }

        public void DrawString(string s, float x, float y)
        {
            WriteLine("<text x=\"{0}\" y=\"{1}\" font-family=\"{2}\" font-size=\"{3}\">{4}</text>",
                x, y + fLastFont.Size, fLastFont.FontFamilyName,
                fLastFont.Size * 3 / 2,
                s.Replace("&", "&amp;").Replace("<", "&lt;").Replace(">", "&gt;"));
        }

        public void DrawImage(IImage img, float x, float y, float width, float height)
        {
        }

        public IImage ImageFromFile(string filename)
        {
            return null;
        }
    }
}