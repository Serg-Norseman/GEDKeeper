/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2017 by Sergey V. Zhdanovskih.
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

using System;
using System.ComponentModel;
using System.Drawing;
using System.Windows.Forms;

namespace GKCommon.Controls
{
    public class DropDownControl : UserControl
    {
        public enum DropDownState
        {
            Closed,
            Closing,
            Dropping,
            Dropped
        }

        private readonly IContainer components;

        private Rectangle fAnchorClientBounds;
        private Size fAnchorSize = new Size(121, 21);
        private bool fClosedWhileInControl;
        private bool fDesignView = true;
        private DropDownContainer fDropContainer;
        private Control fDropDownItem;
        private DropDownState fDropState;
        private bool fMousePressed;
        private Size fStoredSize;
        private string fText;

        public event EventHandler PropertyChanged;

        protected DropDownState DropState
        {
            get { return fDropState; }
        }

        public new string Text
        {
            get { return fText; }
            set
            {
                fText = value;
                Invalidate();
            }
        }

        public DropDownControl()
        {
            components = new Container();
            AutoScaleMode = AutoScaleMode.Font;

            fStoredSize = Size;

            BackColor = Color.White;
            Text = this.Name;
        }

        protected override void Dispose(bool disposing)
        {
            if (disposing && (components != null))
            {
                components.Dispose();
            }
            base.Dispose(disposing);
        }

        public void InitializeDropDown(Control dropDownItem)
        {
            if (fDropDownItem != null)
                throw new Exception("The drop down item has already been implemented!");

            fDesignView = false;
            fDropState = DropDownState.Closed;

            Size = fAnchorSize;
            fAnchorClientBounds = new Rectangle(2, 2, fAnchorSize.Width - 21, fAnchorSize.Height - 4);

            // removes the dropDown item from the controls list so it
            // won't be seen until the drop-down window is active
            if (Controls.Contains(dropDownItem))
                Controls.Remove(dropDownItem);
            fDropDownItem = dropDownItem;
        }

        public Size AnchorSize
        {
            get { return fAnchorSize; }
            set
            {
                fAnchorSize = value;
                Invalidate();
            }
        }

        [DefaultValue(false)]
        protected bool DesignView
        {
            get { return fDesignView; }
            set
            {
                if (fDesignView == value) return;

                fDesignView = value;
                if (fDesignView)
                {
                    Size = fStoredSize;
                }
                else
                {
                    fStoredSize = Size;
                    Size = fAnchorSize;
                }

            }
        }

        protected void OnPropertyChanged()
        {
            if (PropertyChanged != null)
                PropertyChanged(null, null);
        }

        public Rectangle AnchorClientBounds
        {
            get { return fAnchorClientBounds; }
        }

        protected override void OnResize(EventArgs e)
        {
            base.OnResize(e);

            if (fDesignView)
                fStoredSize = Size;
            fAnchorSize.Width = Width;

            if (fDesignView) return;
            fAnchorSize.Height = Height;
            fAnchorClientBounds = new Rectangle(2, 2, fAnchorSize.Width - 21, fAnchorSize.Height - 4);
        }
        
        protected override void OnMouseDown(MouseEventArgs e)
        {
            base.OnMouseDown(e);
            fMousePressed = true;
            OpenDropDown();
        }

        protected override void OnMouseUp(MouseEventArgs e)
        {
            base.OnMouseUp(e);
            fMousePressed = false;
            Invalidate();
        }

        protected virtual bool CanDrop
        {
            get
            {
                if (fDropContainer != null) {
                    return false;
                } else {
                    if (fClosedWhileInControl) {
                        fClosedWhileInControl = false;
                        return false;
                    }
                }

                return !fClosedWhileInControl;
            }
        }

        protected void OpenDropDown()
        {
            if (fDropDownItem == null)
                throw new NotImplementedException("The drop down item has not been initialized! Use the InitializeDropDown() method to do so.");

            if (!CanDrop) return;

            fDropContainer = new DropDownContainer(fDropDownItem);
            fDropContainer.Bounds = GetDropDownBounds();
            fDropContainer.DropStateChange += dropContainer_DropStateChange;
            fDropContainer.FormClosed += dropContainer_Closed;
            ParentForm.Move += ParentForm_Move;
            fDropState = DropDownState.Dropping;
            fDropContainer.Show(this);
            fDropState = DropDownState.Dropped;

            Invalidate();
        }

        private void ParentForm_Move(object sender, EventArgs e)
        {
            fDropContainer.Bounds = GetDropDownBounds();
        }

        public void CloseDropDown()
        {
            if (fDropContainer == null) return;

            fDropState = DropDownState.Closing;
            fDropContainer.Freeze = false;
            fDropContainer.Close();
        }

        private void dropContainer_DropStateChange(DropDownState state)
        {
            fDropState = state;
        }

        private void dropContainer_Closed(object sender, FormClosedEventArgs e)
        {
            if (!fDropContainer.IsDisposed)
            {
                fDropContainer.DropStateChange -= dropContainer_DropStateChange;
                fDropContainer.FormClosed -= dropContainer_Closed;
                ParentForm.Move -= ParentForm_Move;
                fDropContainer.Dispose();
            }
            fDropContainer = null;
            fClosedWhileInControl = (RectangleToScreen(ClientRectangle).Contains(Cursor.Position));
            fDropState = DropDownState.Closed;

            Invalidate();
        }

        protected virtual Rectangle GetDropDownBounds()
        {
            Size inflatedDropSize = new Size(/*fDropDownItem.Width + 2*/Width, fDropDownItem.Height + 2);
            Rectangle screenBounds = new Rectangle(Parent.PointToScreen(new Point(Bounds.X, Bounds.Bottom)), inflatedDropSize);
            Rectangle workingArea = Screen.GetWorkingArea(screenBounds);

            // make sure we're completely in the top-left working area
            if (screenBounds.X < workingArea.X) screenBounds.X = workingArea.X;
            if (screenBounds.Y < workingArea.Y) screenBounds.Y = workingArea.Y;

            // make sure we're not extended past the working area's right /bottom edge
            if (screenBounds.Right > workingArea.Right && workingArea.Width > screenBounds.Width)
                screenBounds.X = workingArea.Right - screenBounds.Width;
            if (screenBounds.Bottom > workingArea.Bottom && workingArea.Height > screenBounds.Height)
                screenBounds.Y = workingArea.Bottom - screenBounds.Height;

            return screenBounds;
        }

        protected override void OnPaint(PaintEventArgs e)
        {
            base.OnPaint(e);

            // Check if VisualStyles are supported...
            // Thanks to codeproject member: Mathiyazhagan for catching this. :)
            if (ComboBoxRenderer.IsSupported)
            {
                ComboBoxRenderer.DrawTextBox(e.Graphics, new Rectangle(new Point(0, 0), fAnchorSize), GetState());
                ComboBoxRenderer.DrawDropDownButton(e.Graphics, new Rectangle(fAnchorSize.Width - 19, 2, 18, fAnchorSize.Height - 4), GetState());
            }
            else
            {
                ControlPaint.DrawComboButton(e.Graphics,
                                             new Rectangle(fAnchorSize.Width - 19, 2, 18, fAnchorSize.Height - 4),
                                             (Enabled) ? ButtonState.Normal : ButtonState.Inactive);
            }

            using (Brush b = new SolidBrush(BackColor))
            {
                e.Graphics.FillRectangle(b, AnchorClientBounds);
            }

            TextRenderer.DrawText(e.Graphics, fText, Font, AnchorClientBounds, ForeColor, TextFormatFlags.WordEllipsis);
        }

        private System.Windows.Forms.VisualStyles.ComboBoxState GetState()
        {
            if (fMousePressed || fDropContainer != null)
                return System.Windows.Forms.VisualStyles.ComboBoxState.Pressed;
            else
                return System.Windows.Forms.VisualStyles.ComboBoxState.Normal;
        }

        public void FreezeDropDown(bool remainVisible)
        {
            if (fDropContainer == null) return;

            fDropContainer.Freeze = true;
            if (!remainVisible)
                fDropContainer.Visible = false;
        }

        public void UnFreezeDropDown()
        {
            if (fDropContainer == null) return;

            fDropContainer.Freeze = false;
            if (!fDropContainer.Visible)
                fDropContainer.Visible = true;
        }


        internal sealed class DropDownContainer : Form, IMessageFilter
        {
            private readonly Control fDropDownItem;

            public delegate void DropWindowArgs(DropDownState state);

            public bool Freeze;

            public event DropWindowArgs DropStateChange;

            public DropDownContainer(Control dropDownItem)
            {
                fDropDownItem = dropDownItem;

                FormBorderStyle = FormBorderStyle.None;
                dropDownItem.Location = new Point(1, 1);
                Controls.Add(dropDownItem);
                StartPosition = FormStartPosition.Manual;
                ShowInTaskbar = false;
                Application.AddMessageFilter(this);
            }

            public bool PreFilterMessage(ref Message m)
            {
                if (!Freeze && Visible && (Form.ActiveForm == null || !Form.ActiveForm.Equals(this)))
                {
                    OnDropStateChange(DropDownState.Closing);
                    Close();
                }

                return false;
            }

            private void OnDropStateChange(DropDownState state)
            {
                var dropStateChange = DropStateChange;
                if (dropStateChange != null)
                    dropStateChange(state);
            }

            protected override void OnPaint(PaintEventArgs e)
            {
                base.OnPaint(e);
                e.Graphics.DrawRectangle(Pens.Gray, new Rectangle(0,0, ClientSize.Width - 1, ClientSize.Height - 1));
            }

            protected override void OnResize(EventArgs e)
            {
                base.OnResize(e);

                if (fDropDownItem != null) {
                    fDropDownItem.Width = Width;
                }
            }

            protected override void OnClosing(CancelEventArgs e)
            {
                Application.RemoveMessageFilter(this);
                Controls.RemoveAt(0); // prevent the control from being disposed
                base.OnClosing(e);
            }
        }
    }
}
