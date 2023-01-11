/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2023 by Sergey V. Zhdanovskih.
 *
 *  This file is part of "GEDKeeper".
 *
 *  This program is free software: you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation, either version 3 of the License, or
 *  (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

#if MONO
#define STD_VIEW
#endif

using System;
using System.ComponentModel;
using System.Drawing;
using System.Drawing.Drawing2D;
using System.Resources;
using System.Windows.Forms;
using GKCore;

namespace GKUI.Components
{
    [ToolboxBitmap(typeof(TabControl))]
    public class GKTabControl : TabControl
    {
        private TabControlAppearance fAppearance;

        public TabControlAppearance Appearance
        {
            get { return fAppearance; }
        }

#if !STD_VIEW

        private const int TAB_PADDING = 5;

        private SubClass scUpDown = null;
        private bool bUpDown;
        private ImageList leftRightImages = null;
        private const int nMargin = 5;
        private bool fCloseableTabs;
        private Point fMouseLocation;


        new public TabAlignment Alignment
        {
            get { return base.Alignment; }
            set {
                TabAlignment ta = value;
                if ((ta != TabAlignment.Top) && (ta != TabAlignment.Bottom))
                    ta = TabAlignment.Top;

                base.Alignment = ta;
            }
        }

        public bool CloseableTabs
        {
            get { return fCloseableTabs; }
            set {
                if (fCloseableTabs != value) {
                    fCloseableTabs = value;

                    if (fCloseableTabs) {
                        Padding = new Point(20, 4);
                    } else {
                        Padding = new Point(6, 3);
                    }

                    Invalidate();
                }
            }
        }


        public GKTabControl()
        {
            fAppearance = new TabControlAppearance(this);

            SetStyle(ControlStyles.UserPaint, true);
            SetStyle(ControlStyles.AllPaintingInWmPaint, true);
            SetStyle(ControlStyles.DoubleBuffer, true);
            SetStyle(ControlStyles.ResizeRedraw, true);
            SetStyle(ControlStyles.SupportsTransparentBackColor, true);

            fCloseableTabs = false; 
            bUpDown = false;

            ControlAdded += new ControlEventHandler(FlatTabControl_ControlAdded);
            ControlRemoved += new ControlEventHandler(FlatTabControl_ControlRemoved);
            SelectedIndexChanged += new EventHandler(FlatTabControl_SelectedIndexChanged);

            leftRightImages = new ImageList();
            //leftRightImages.ImageSize = new Size(16, 16); // default

            ResourceManager resources = new ResourceManager(typeof(GKTabControl));
            Bitmap updownImage = ((Bitmap)(resources.GetObject("TabIcons.bmp")));

            if (updownImage != null) {
                updownImage.MakeTransparent(Color.White);
                leftRightImages.Images.AddStrip(updownImage);
            }
        }

        protected override void Dispose(bool disposing)
        {
            if (disposing) {
                leftRightImages.Dispose();
            }
            base.Dispose(disposing);
        }

        protected override void OnPaint(PaintEventArgs e)
        {
            base.OnPaint(e);

            if (!Visible)
                return;

            var g = e.Graphics;
            Rectangle tabControlArea = ClientRectangle;
            Rectangle tabArea = DisplayRectangle;

            // fill client area
            using (Brush br = new SolidBrush(fAppearance.BackColor)) {
                g.FillRectangle(br, tabControlArea);

            }

            // draw border
            using (Pen border = new Pen(fAppearance.TabBorder)) {
                int nDelta = SystemInformation.Border3DSize.Width;
                tabArea.Inflate(nDelta, nDelta);
                g.DrawRectangle(border, tabArea);
            }

            // clip region for drawing tabs
            Region rsaved = g.Clip;
            Rectangle rreg;

            int nWidth = tabArea.Width + nMargin;
            if (bUpDown) {
                // exclude updown control for painting
                if (Win32.IsWindowVisible(scUpDown.Handle)) {
                    Rectangle rupdown = new Rectangle();
                    Win32.GetWindowRect(scUpDown.Handle, ref rupdown);
                    Rectangle rupdown2 = RectangleToClient(rupdown);

                    nWidth = rupdown2.X;
                }
            }

            rreg = new Rectangle(tabArea.Left, tabControlArea.Top, nWidth - nMargin, tabControlArea.Height);

            g.SetClip(rreg);

            // draw tabs
            for (int i = 0; i < TabCount; i++)
                DrawTab(g, TabPages[i], i);

            g.Clip = rsaved;

            // draw background to cover flat border areas
            if (SelectedTab != null) {
                using (var border = new Pen(fAppearance.Tab)) {
                    tabArea.Offset(1, 1);
                    tabArea.Width -= 2;
                    tabArea.Height -= 2;

                    g.DrawRectangle(border, tabArea);
                    tabArea.Width -= 1;
                    tabArea.Height -= 1;
                    g.DrawRectangle(border, tabArea);
                }
            }
        }

        protected override void OnMouseLeave(EventArgs e)
        {
            base.OnMouseLeave(e);
            fMouseLocation = Point.Empty;
        }

        protected override void OnMouseMove(MouseEventArgs e)
        {
            base.OnMouseMove(e);
            fMouseLocation = e.Location;
        }

        internal void DrawTab(Graphics g, TabPage tabPage, int nIndex)
        {
            Rectangle tabRect = GetTabRect(nIndex);
            RectangleF tabTextArea = (RectangleF)GetTabRect(nIndex);

            bool highlighted = tabRect.Contains(fMouseLocation);
            bool selected = (SelectedIndex == nIndex);
            if (!selected) {
                tabRect.Y += 2;
                tabRect.Height -= 2;
            }

            Point[] pt = new Point[7];
            if (Alignment == TabAlignment.Top) {
                pt[0] = new Point(tabRect.Left, tabRect.Bottom);
                pt[1] = new Point(tabRect.Left, tabRect.Top + 2);
                pt[2] = new Point(tabRect.Left + 2, tabRect.Top);
                pt[3] = new Point(tabRect.Right - 2, tabRect.Top);
                pt[4] = new Point(tabRect.Right, tabRect.Top + 2);
                pt[5] = new Point(tabRect.Right, tabRect.Bottom);
                pt[6] = new Point(tabRect.Left, tabRect.Bottom);
            } else {
                pt[0] = new Point(tabRect.Left, tabRect.Top);
                pt[1] = new Point(tabRect.Right, tabRect.Top);
                pt[2] = new Point(tabRect.Right, tabRect.Bottom - 2);
                pt[3] = new Point(tabRect.Right - 2, tabRect.Bottom);
                pt[4] = new Point(tabRect.Left + 2, tabRect.Bottom);
                pt[5] = new Point(tabRect.Left, tabRect.Bottom - 2);
                pt[6] = new Point(tabRect.Left, tabRect.Top);
            }

            var tabColor = (selected) ? fAppearance.TabSelected : ((highlighted) ? fAppearance.TabHighlight : fAppearance.Tab);

            // fill this tab with background color
            Brush br = new SolidBrush(tabColor);
            g.FillPolygon(br, pt);
            br.Dispose();

            // draw border
            using (Pen border = new Pen(fAppearance.TabBorder)) {
                g.DrawPolygon(border, pt);
            }

            if (selected) {
                // clear bottom lines
                Pen pen = new Pen(tabPage.BackColor);

                switch (Alignment) {
                    case TabAlignment.Top:
                        g.DrawLine(pen, tabRect.Left + 1, tabRect.Bottom, tabRect.Right - 1, tabRect.Bottom);
                        g.DrawLine(pen, tabRect.Left + 1, tabRect.Bottom + 1, tabRect.Right - 1, tabRect.Bottom + 1);
                        break;

                    case TabAlignment.Bottom:
                        g.DrawLine(pen, tabRect.Left + 1, tabRect.Top, tabRect.Right - 1, tabRect.Top);
                        g.DrawLine(pen, tabRect.Left + 1, tabRect.Top - 1, tabRect.Right - 1, tabRect.Top - 1);
                        g.DrawLine(pen, tabRect.Left + 1, tabRect.Top - 2, tabRect.Right - 1, tabRect.Top - 2);
                        break;
                }

                pen.Dispose();
            }

            // draw tab's icon
            if ((tabPage.ImageIndex >= 0) && (ImageList != null) && (ImageList.Images[tabPage.ImageIndex] != null)) {
                int nLeftMargin = 8;
                int nRightMargin = 2;

                Image img = ImageList.Images[tabPage.ImageIndex];

                Rectangle rimage = new Rectangle(tabRect.X + nLeftMargin, tabRect.Y + 1, img.Width, img.Height);

                // adjust rectangles
                float nAdj = (float)(nLeftMargin + img.Width + nRightMargin);

                rimage.Y += (tabRect.Height - img.Height) / 2;
                tabTextArea.X += nAdj;
                tabTextArea.Width -= nAdj;

                // draw icon
                g.DrawImage(img, rimage);
            }

            // draw string
            StringFormat stringFormat = new StringFormat();
            stringFormat.Alignment = StringAlignment.Center;
            stringFormat.LineAlignment = StringAlignment.Center;

            br = new SolidBrush(tabPage.ForeColor);

            g.DrawString(tabPage.Text, Font, br, tabTextArea, stringFormat);

            // if closeable
            /*
                var closeRect = GetCloseButtonRect(innerRec);

                // Draw the close button.
                //if (tab.HasCloseButton)
                {
                    Image closeImg = UIHelper.LoadResourceImage("Resources.btn_close.png");

                    if (closeImg != null) {
                        gfx.DrawImageUnscaled(closeImg, closeRect.Left, closeRect.Top + innerRec.Y);
                    } else {
                        using (var xFont = new Font(e.Font, FontStyle.Bold)) {
                            Font font = (isSelected) ? xFont : e.Font;
                            gfx.DrawString("x", font, Brushes.Black, closeRect.Left, closeRect.Top);
                        }
                    }
                }

                // Draw the text. If the text is too long then cut off the end and add '...'
                // To avoid this behaviour, set the TabControl.ItemSize to larger value.

                var tabFont = isSelected ? new Font(e.Font, FontStyle.Bold) : e.Font;
                var tabTextSize = gfx.MeasureString(tabText, tabFont);

                var textX = iconX + TAB_PADDING;
                var textY = (innerRec.Height - tabTextSize.Height + innerRec.Y) / 2;

                // Calculate if the text fits as is. If not then trim it.
                if (textX + tabTextSize.Width > closeRect.Left - TAB_PADDING)
                    tabText = TrimTextToFit(gfx, tabText.Substring(0, tabText.Length - 1), tabFont, closeRect.Left - TAB_PADDING - textX);

                gfx.DrawString(tabText, tabFont, Brushes.Black, textX, textY + innerRec.Y);
             */
        }

        internal void DrawIcons(Graphics g)
        {
            if ((leftRightImages == null) || (leftRightImages.Images.Count != 4))
                return;

            // calc positions
            Rectangle TabControlArea = ClientRectangle;

            Rectangle r0 = new Rectangle();
            Win32.GetClientRect(scUpDown.Handle, ref r0);

            Brush br = new SolidBrush(SystemColors.Control);
            g.FillRectangle(br, r0);
            br.Dispose();

            Pen border = new Pen(SystemColors.ControlDark);
            Rectangle rborder = r0;
            rborder.Inflate(-1, -1);
            g.DrawRectangle(border, rborder);
            border.Dispose();

            int nMiddle = (r0.Width / 2);
            int nTop = (r0.Height - 16) / 2;
            int nLeft = (nMiddle - 16) / 2;

            Rectangle r1 = new Rectangle(nLeft, nTop, 16, 16);
            Rectangle r2 = new Rectangle(nMiddle + nLeft, nTop, 16, 16);

            // draw buttons
            Image img = leftRightImages.Images[1];
            if (img != null) {
                if (TabCount > 0) {
                    Rectangle r3 = GetTabRect(0);
                    if (r3.Left < TabControlArea.Left)
                        g.DrawImage(img, r1);
                    else {
                        img = leftRightImages.Images[3];
                        if (img != null)
                            g.DrawImage(img, r1);
                    }
                }
            }

            img = leftRightImages.Images[0];
            if (img != null) {
                if (TabCount > 0) {
                    Rectangle r3 = GetTabRect(TabCount - 1);
                    if (r3.Right > (TabControlArea.Width - r0.Width))
                        g.DrawImage(img, r2);
                    else {
                        img = leftRightImages.Images[2];
                        if (img != null)
                            g.DrawImage(img, r2);
                    }
                }
            }
        }

        private static string TrimTextToFit(Graphics g, string text, Font font, int width)
        {
            if (string.IsNullOrEmpty(text))
                return text;

            string txtToFit = text + "...";
            if (g.MeasureString(txtToFit, font).Width <= width)
                return txtToFit;
            else
                // Remove the last character and try again.
                return TrimTextToFit(g, text.Substring(0, text.Length - 1), font, width);
        }

        private Rectangle GetCloseButtonRect(Rectangle tabRect)
        {
            Image closeImg = UIHelper.LoadResourceImage("Resources.btn_close.png");
            int closeX = tabRect.Right - TAB_PADDING - closeImg.Width;
            int closeY = (tabRect.Height - closeImg.Height + tabRect.Y) / 2;
            var closeButton = new Rectangle(closeX, closeY, closeImg.Width, closeImg.Height);
            return closeButton;
        }

        protected override void OnMouseDown(MouseEventArgs e)
        {
            base.OnMouseDown(e);

            if (fCloseableTabs && e.Button == MouseButtons.Left) {
                Rectangle closeButton = GetCloseButtonRect(GetTabRect(SelectedIndex));
                if (closeButton.Contains(e.Location)) {
                    TabPages.Remove(SelectedTab);
                }
            }
        }

        protected override void OnFontChanged(EventArgs e)
        {
            base.OnFontChanged(e);

            Win32.SendMessage(Handle, Win32.WM_SETFONT, Font.ToHfont(), (IntPtr)(-1));
            Win32.SendMessage(Handle, Win32.WM_FONTCHANGE, IntPtr.Zero, IntPtr.Zero);
            //UpdateStyles();
        }

        protected override void OnCreateControl()
        {
            base.OnCreateControl();

            OnFontChanged(EventArgs.Empty);

            FindUpDown();
        }

        private void FlatTabControl_ControlAdded(object sender, ControlEventArgs e)
        {
            FindUpDown();
            UpdateUpDown();
        }

        private void FlatTabControl_ControlRemoved(object sender, ControlEventArgs e)
        {
            FindUpDown();
            UpdateUpDown();
        }

        private void FlatTabControl_SelectedIndexChanged(object sender, EventArgs e)
        {
            UpdateUpDown();
            Invalidate();   // we need to update border and background colors
        }

        private void FindUpDown()
        {
            bool bFound = false;

            // find the UpDown control
            IntPtr pWnd = Win32.GetWindow(Handle, Win32.GW_CHILD);

            while (pWnd != IntPtr.Zero) {
                // Get the window class name
                char[] className = new char[33];

                int length = Win32.GetClassName(pWnd, className, 32);

                string s = new string(className, 0, length);

                if (s == "msctls_updown32") {
                    bFound = true;

                    if (!bUpDown) {
                        // Subclass it
                        scUpDown = new SubClass(pWnd, true);
                        scUpDown.SubClassedWndProc += new SubClass.SubClassWndProcEventHandler(scUpDown_SubClassedWndProc);

                        bUpDown = true;
                    }
                    break;
                }

                pWnd = Win32.GetWindow(pWnd, Win32.GW_HWNDNEXT);
            }

            if ((!bFound) && (bUpDown))
                bUpDown = false;
        }

        private void UpdateUpDown()
        {
            if (bUpDown) {
                if (Win32.IsWindowVisible(scUpDown.Handle)) {
                    Rectangle rect = new Rectangle();

                    Win32.GetClientRect(scUpDown.Handle, ref rect);
                    Win32.InvalidateRect(scUpDown.Handle, ref rect, true);
                }
            }
        }

        private int scUpDown_SubClassedWndProc(ref Message m)
        {
            switch (m.Msg) {
                case Win32.WM_PAINT: {
                        // redraw
                        IntPtr hDC = Win32.GetWindowDC(scUpDown.Handle);
                        Graphics g = Graphics.FromHdc(hDC);

                        DrawIcons(g);

                        g.Dispose();
                        Win32.ReleaseDC(scUpDown.Handle, hDC);

                        // return 0 (processed)
                        m.Result = IntPtr.Zero;

                        // validate current rect
                        Rectangle rect = new Rectangle();

                        Win32.GetClientRect(scUpDown.Handle, ref rect);
                        Win32.ValidateRect(scUpDown.Handle, ref rect);
                    }
                    return 1;
            }

            return 0;
        }

#else

        public GKTabControl()
        {
            fAppearance = new TabControlAppearance(this);
        }

#endif
    }


    public class TabControlAppearance
    {
        private readonly TabControl fOwner;
        private Color fBackColor;
        private Color fTab;
        private Color fTabBorder;
        private Color fTabHighlight;
        private Color fTabSelected;

        public Color BackColor
        {
            get {
                return fBackColor;
            }
            set {
                if (fBackColor != value) {
                    fBackColor = value;
                    fOwner.Invalidate();
                }
            }
        }

        public Color Tab
        {
            get {
                return fTab;
            }
            set {
                if (fTab != value) {
                    fTab = value;
                    fOwner.Invalidate();
                }
            }
        }

        public Color TabBorder
        {
            get {
                return fTabBorder;
            }
            set {
                if (fTabBorder != value) {
                    fTabBorder = value;
                    fOwner.Invalidate();
                }
            }
        }

        public Color TabHighlight
        {
            get {
                return fTabHighlight;
            }
            set {
                if (fTabHighlight != value) {
                    fTabHighlight = value;
                    fOwner.Invalidate();
                }
            }
        }

        public Color TabSelected
        {
            get {
                return fTabSelected;
            }
            set {
                if (fTabSelected != value) {
                    fTabSelected = value;
                    fOwner.Invalidate();
                }
            }
        }

        public TabControlAppearance(TabControl owner)
        {
            fOwner = owner;
            Reset(false);
        }

        public void Reset(bool invalidate = true)
        {
            fBackColor = SystemColors.Control;
            fTab = SystemColors.Control;
            fTabBorder = SystemColors.ControlDark;
            fTabHighlight = Color.FromArgb(216, 234, 249);
            fTabSelected = SystemColors.ButtonHighlight;

            if (invalidate)
                fOwner.Invalidate();
        }
    }
}
