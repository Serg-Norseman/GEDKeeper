/*
 *  An HyperText (readonly) Memo for Delphi
 *    from TJumpMemo by Alexander Kuznetsov (sanhome@hotmail.com)
 *  Copyright (C) 1997 Paul Toth (TothPaul@Mygale.org)
 *  http://www.mygale.org/~tothpaul
 *
 *  This program is free software; you can redistribute it and/or
 *  modify it under the terms of the GNU General Public License
 *  as published by the Free Software Foundation; either version 2
 *  of the License, or (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this program; if not, write to the Free Software
 *  Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.
 *
 *  C# implementation:
 *  Copyright (C) 2011 by Sergey V. Zhdanovskih.
 */

using System;
using System.Collections.Generic;
using System.Drawing;
using System.Windows.Forms;

namespace GKCommon.Controls
{
    public delegate void LinkEventHandler(object sender, string linkName);

    public enum RuleStyle
    {
        rsLowered,
        rsRaised
    }

    /// <summary>
    /// 
    /// </summary>
    public class HyperView : GKScrollableControl
    {
        private sealed class HyperLink
        {
            private readonly ExtRect fRect;

            public readonly string Name;

            public HyperLink(string name, int x, int y, int w, int h)
            {
                Name = name;
                fRect = ExtRect.CreateBounds(x, y, w, h);
            }

            public bool HasCoord(int x, int y, int xOffset, int yOffset)
            {
                return x >= fRect.Left + xOffset && x <= fRect.Right + xOffset
                    && y >= fRect.Top + yOffset && y <= fRect.Bottom + yOffset;
            }
        }

        //private bool fAcceptFontChange;
        private int fBorderWidth;
        private Color fColor;
        private SolidBrush fDefBrush;
        private int[] fHeights;
        private Size fTextSize;
        private readonly StringList fLines;
        private readonly List<HyperLink> fLinks;
        private int fLink;
        private Color fLinkColor;
        private Font fTextFont;
        private RuleStyle fRuleStyle;

        private static readonly object EventLink;

        static HyperView()
        {
            EventLink = new object();
        }

        public event LinkEventHandler OnLink
        {
            add { Events.AddHandler(EventLink, value); }
            remove { Events.RemoveHandler(EventLink, value); }
        }

        public int BorderWidth
        {
            get { return fBorderWidth; }
            set {
                if (fBorderWidth == value) return;

                fBorderWidth = value;
                Invalidate();
            }
        }

        public Color Color
        {
            get { return fColor; }
            set {
                if (fColor == value) return;

                fColor = value;
                Invalidate();
            }
        }

        public StringList Lines
        {
            get { return fLines; }
            set { fLines.Assign(value); }
        }

        public Color LinkColor
        {
            get { return fLinkColor; }
            set { fLinkColor = value; }
        }

        public RuleStyle RuleStyle
        {
            get { return fRuleStyle; }
            set {
                if (fRuleStyle == value) return;

                fRuleStyle = value;
                Invalidate();
            }
        }


        public HyperView() : base()
        {
            SetStyle(ControlStyles.AllPaintingInWmPaint | ControlStyles.UserPaint | ControlStyles.OptimizedDoubleBuffer | ControlStyles.ResizeRedraw, true);
            UpdateStyles();

            BorderStyle = BorderStyle.Fixed3D;
            DoubleBuffered = true;
            TabStop = true;

            //fAcceptFontChange = false;
            fLines = new StringList();
            fLines.OnChange += LinesChanged;
            fHeights = new int[0];
            fColor = SystemColors.Control;
            fLinks = new List<HyperLink>();
            fLinkColor = Color.Blue;
            fLink = -1;
            fTextSize = Size.Empty;
        }

        protected override void Dispose(bool disposing)
        {
            if (disposing)
            {
                fLinks.Clear();
                //FLinks.Dispose();

                fHeights = null;
                fLines.Dispose();

                if (fDefBrush != null) fDefBrush.Dispose();
                if (fTextFont != null) fTextFont.Dispose();
            }
            base.Dispose(disposing);
        }

        private void LinesChanged(object sender)
        {
            AutoScrollPosition = new Point(0, 0);
            ArrangeText();
        }

        private static int GetFontSize(string s, ref int i)
        {
            int result = 0;

            while (true)
            {
                char c = s[i];
                if (c < '0' || c > '9') break;

                i++;
                result = 10 * result + (int)s[i - 1] - 48;
            }

            return result;
        }

        /*private Color GetFontColor(string s, ref int i)
		{
			string ss = new string(s[i - 1], 1);
			while (s[i] != '~')
			{
				i++;
				ss += s[i - 1];
			}
			return Color.FromName(ss);
		}*/

        private void MeasureText(Graphics grx, string ss, ref int xPos, ref int yPos, ref int hMax, ref int xMax)
        {
            if (yPos >= -hMax && ss != "") {
                Size strSize = grx.MeasureString(ss, fTextFont).ToSize();
                xPos += strSize.Width;

                if (xPos > xMax) xMax = xPos;
                int h = strSize.Height;
                if (h > hMax) hMax = h;
            }
        }

        private void OutText(Graphics gfx, string ss, ref int xPos, ref int yPos, ref int hMax)
        {
            if (yPos >= -hMax && ss != "") {
                gfx.DrawString(ss, fTextFont, fDefBrush, xPos, yPos);

                Size strSize = gfx.MeasureString(ss, fTextFont).ToSize();
                xPos += strSize.Width;
            }
        }

        private void ArrangeText()
        {
            try {
                fTextFont = (Parent.Font.Clone() as Font);
                fDefBrush = new SolidBrush(Color.Black);
                fHeights = new int[fLines.Count];
                //fAcceptFontChange = false;
                fLinks.Clear();

                Graphics gfx = CreateGraphics();
                try
                {
                    int yPos = 0;
                    int xMax = 0;
                    //▼▲↔∟←→↓↑↨▬§¶‼↕◄►☼♫♪♀♂◙○◘◄•♠♣♦♥☻☺

                    int num = fLines.Count;
                    for (int line = 0; line < num; line++)
                    {
                        int xPos = 0;
                        int lineHeight = gfx.MeasureString("A", fTextFont).ToSize().Height;

                        string s = fLines[line];

                        int i = 1;
                        string ss = "";
                        while (i <= s.Length)
                        {
                            if (s[i - 1] == '~')
                            {
                                if (s[i] == '~') {
                                    ss += "~";
                                }

                                MeasureText(gfx, ss, ref xPos, ref yPos, ref lineHeight, ref xMax);
                                i++;

                                while (s[i - 1] != '~')
                                {
                                    char c = char.ToUpper(s[i - 1]);

                                    switch (c)
                                    {
                                        case '+':
                                            SetFontSize((fTextFont.Size + GetFontSize(s, ref i)));
                                            break;

                                        case '-':
                                            SetFontSize((fTextFont.Size - GetFontSize(s, ref i)));
                                            break;

                                        case '0':
                                            fTextFont = (Parent.Font.Clone() as Font);
                                            break;

                                        case 'B':
                                            // [b][/b]
                                            SetFontStyle(FontStyle.Bold);
                                            break;

                                        case 'I':
                                            // [i][/i]
                                            SetFontStyle(FontStyle.Italic);
                                            break;

                                        case 'R':
                                            // dummy
                                            break;

                                        case 'S':
                                            // [s][/s]
                                            SetFontStyle(FontStyle.Strikeout);
                                            break;

                                        case 'U':
                                            // [u][/u]
                                            SetFontStyle(FontStyle.Underline);
                                            break;

                                        case '^':
                                            // [url][/url] or [url=...][/url]
                                            {
                                                string sn = "";
                                                while (s[i] != ':') {
                                                    i++;
                                                    sn += s[i - 1];
                                                }
                                                i++;
                                                ss = "";
                                                while (s[i] != '~') {
                                                    i++;
                                                    ss += s[i - 1];
                                                }

                                                int ssWidth = gfx.MeasureString(ss, fTextFont).ToSize().Width;
                                                fLinks.Add(new HyperLink(sn, xPos, yPos, ssWidth, lineHeight));
                                                MeasureText(gfx, ss, ref xPos, ref yPos, ref lineHeight, ref xMax);

                                                break;
                                            }

                                        default:
                                            while (s[i] != '~') i++;
                                            break;
                                    }

                                    i++;
                                }
                                ss = "";
                            }
                            else
                            {
                                ss += s[i - 1];
                            }

                            i++;
                        }

                        MeasureText(gfx, ss, ref xPos, ref yPos, ref lineHeight, ref xMax);
                        yPos += lineHeight;
                        fHeights[line] = lineHeight;
                    }

                    fTextSize = new Size(xMax + 2 * fBorderWidth, yPos + 2 * fBorderWidth);
                }
                finally
                {
                    gfx.Dispose();
                }

                //fAcceptFontChange = true;
                AdjustViewPort(fTextSize);
            }
            catch (Exception ex)
            {
                Logger.LogWrite("HyperView.ArrangeText(): " + ex.Message);
            }
        }

        private void DoPaint(Graphics gfx)
        {
            try {
                if (fHeights.Length != fLines.Count) return;

                //fAcceptFontChange = false;
                try
                {
                    Rectangle clientRect = ClientRectangle;
                    gfx.FillRectangle(new SolidBrush(SystemColors.Control), clientRect);

                    int yOffset = fBorderWidth - -AutoScrollPosition.Y;

                    int num = fLines.Count;
                    for (int line = 0; line < num; line++)
                    {
                        int xOffset = fBorderWidth - -AutoScrollPosition.X;
                        int lineHeight = fHeights[line];

                        string s = fLines[line];

                        int i = 1;
                        string ss = "";
                        while (i <= s.Length)
                        {
                            if (s[i - 1] == '~')
                            {
                                if (s[i] == '~') {
                                    ss += "~";
                                }

                                OutText(gfx, ss, ref xOffset, ref yOffset, ref lineHeight);
                                i++;

                                while (s[i - 1] != '~')
                                {
                                    char c = char.ToUpper(s[i - 1]);

                                    switch (c)
                                    {
                                        case '+':
                                            SetFontSize((fTextFont.Size + GetFontSize(s, ref i)));
                                            break;

                                        case '-':
                                            SetFontSize((fTextFont.Size - GetFontSize(s, ref i)));
                                            break;

                                        case '0':
                                            fTextFont = (Parent.Font.Clone() as Font);
                                            break;

                                        case 'B':
                                            SetFontStyle(FontStyle.Bold);
                                            break;

                                        case 'I':
                                            SetFontStyle(FontStyle.Italic);
                                            break;

                                        case 'R':
                                            int rulerWidth = clientRect.Width - (xOffset * 2);
                                            ControlPaint.DrawBorder3D(gfx,
                                                                      xOffset,
                                                                      yOffset + (lineHeight - 3) / 2,
                                                                      rulerWidth, 3,
                                                                      Border3DStyle.Bump, Border3DSide.All);
                                            break;

                                        case 'S':
                                            SetFontStyle(FontStyle.Strikeout);
                                            break;

                                        case 'U':
                                            SetFontStyle(FontStyle.Underline);
                                            break;

                                        case '^':
                                            {
                                                string sn = "";
                                                while (s[i] != ':') {
                                                    i++;
                                                    sn += s[i - 1];
                                                }
                                                i++;
                                                ss = "";
                                                while (s[i] != '~') {
                                                    i++;
                                                    ss += s[i - 1];
                                                }

                                                Color saveColor = fDefBrush.Color;
                                                fDefBrush.Color = fLinkColor;
                                                SetFontStyle(FontStyle.Underline);

                                                OutText(gfx, ss, ref xOffset, ref yOffset, ref lineHeight);

                                                fDefBrush.Color = saveColor;
                                                SetFontStyle(FontStyle.Underline);

                                                break;
                                            }

                                        default:
                                            while (s[i] != '~') i++;
                                            break;
                                    }

                                    i++;
                                }
                                ss = "";
                            }
                            else
                            {
                                ss += s[i - 1];
                            }

                            i++;
                        }

                        OutText(gfx, ss, ref xOffset, ref yOffset, ref lineHeight);
                        yOffset += lineHeight;
                    }
                }
                finally
                {
                    //fAcceptFontChange = true;
                }
            }
            catch (Exception ex)
            {
                Logger.LogWrite("HyperView.DoPaint(): " + ex.Message);
            }
        }

        private void SetFontSize(float size)
        {
            fTextFont = new Font(fTextFont.Name, size, fTextFont.Style, fTextFont.Unit, fTextFont.GdiCharSet, fTextFont.GdiVerticalFont);
        }

        private void SetFontStyle(FontStyle style)
        {
            FontStyle fontStyle = fTextFont.Style;
            if ((fontStyle & style) == FontStyle.Regular)
            {
                fontStyle |= style;
            }
            else
            {
                fontStyle &= ~style;
            }
            fTextFont = new Font(fTextFont, fontStyle);
        }

        private void GotoLink(int linkIndex)
        {
            HyperLink hLink = fLinks[linkIndex];
            string lnk = "~@" + hLink.Name + "~";
            int h = fBorderWidth;

            int num = fLines.Count;
            for (int j = 0; j < num; j++) {
                if (fLines[j].IndexOf(lnk) >= 0) {
                    AutoScrollPosition = new Point(0, h);
                    //ScrollTo(0, h);
                    return;
                }

                h += fHeights[j];
            }

            DoLink(hLink.Name);
        }

        private void DoLink(string linkName)
        {
            LinkEventHandler eventHandler = (LinkEventHandler)Events[EventLink];
            if (eventHandler != null)
                eventHandler(this, linkName);
        }

        #region Protected methods

        protected override void OnFontChanged(EventArgs e)
        {
            ArrangeText();
            base.OnFontChanged(e);
        }

        protected override void OnKeyDown(KeyEventArgs e)
        {
            base.OnKeyDown(e);

            switch (e.KeyCode)
            {
                case Keys.Prior:
                    AdjustScroll(0, -VerticalScroll.LargeChange);
                    break;

                case Keys.Next:
                    AdjustScroll(0, VerticalScroll.LargeChange);
                    break;

                case Keys.Home:
                    AdjustScroll(-HorizontalScroll.Maximum, -VerticalScroll.Maximum);
                    break;

                case Keys.End:
                    AdjustScroll(-HorizontalScroll.Maximum, VerticalScroll.Maximum);
                    break;

                case Keys.Left:
                    AdjustScroll(-(e.Modifiers == Keys.None ? HorizontalScroll.SmallChange : HorizontalScroll.LargeChange), 0);
                    break;

                case Keys.Right:
                    AdjustScroll(e.Modifiers == Keys.None ? HorizontalScroll.SmallChange : HorizontalScroll.LargeChange, 0);
                    break;

                case Keys.Up:
                    AdjustScroll(0, -(e.Modifiers == Keys.None ? VerticalScroll.SmallChange : VerticalScroll.LargeChange));
                    break;

                case Keys.Down:
                    AdjustScroll(0, e.Modifiers == Keys.None ? VerticalScroll.SmallChange : VerticalScroll.LargeChange);
                    break;
            }
        }

        protected override bool IsInputKey(Keys keyData)
        {
            bool result;

            if ((keyData & Keys.Right) == Keys.Right || (keyData & Keys.Left) == Keys.Left ||
                (keyData & Keys.Up) == Keys.Up || (keyData & Keys.Down) == Keys.Down ||
                (keyData & Keys.Prior) == Keys.Prior || (keyData & Keys.Next) == Keys.Next ||
                (keyData & Keys.End) == Keys.End || (keyData & Keys.Home) == Keys.Home)
                result = true;
            else
                result = base.IsInputKey(keyData);

            return result;
        }

        protected override void OnMouseDown(MouseEventArgs e)
        {
            base.OnMouseDown(e);

            if (fLink >= 0) GotoLink(fLink);
        }

        protected override void OnMouseMove(MouseEventArgs e)
        {
            base.OnMouseMove(e);

            int yOffset = (fBorderWidth - -AutoScrollPosition.Y);
            int xOffset = (fBorderWidth - -AutoScrollPosition.X);

            int num = fLinks.Count;
            for (int i = 0; i < num; i++)
            {
                if (fLinks[i].HasCoord(e.X, e.Y, xOffset, yOffset))
                {
                    fLink = i;
                    Cursor = Cursors.Hand;
                    return;
                }
            }

            if (fLink >= 0)
            {
                Cursor = Cursors.Default;
                fLink = -1;
            }
        }

        protected override void OnPaint(PaintEventArgs e)
        {
            DoPaint(e.Graphics);
            base.OnPaint(e);
        }

        #endregion
    }
}
