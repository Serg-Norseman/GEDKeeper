/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2017 by Sergey V. Zhdanovskih.
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
using System.Runtime.CompilerServices;
using Eto;
using Eto.Drawing;
using Eto.Forms;

namespace GKUI.Components
{
    /// <summary>
    /// 
    /// </summary>
    [Handler(typeof(CustomPanel.IHandler))]
    public class CustomPanel : Scrollable
    {
        #region Temp for compatibility

        public Font Font {
            get;
            set;
        }

        public Color TextColor {
            get;
            set;
        }

        #endregion

        public new interface ICallback : Control.ICallback, Widget.ICallback
        {
            void OnPaint(CustomPanel widget, PaintEventArgs e);
        }

        protected new class Callback : Control.Callback, CustomPanel.ICallback, Control.ICallback, Widget.ICallback
        {
            public void OnPaint(CustomPanel widget, PaintEventArgs e)
            {
                widget.Platform.Invoke(delegate {
                                           widget.OnPaint(e);
                                       });
            }
        }

        [AutoInitialize(false)]
        public new interface IHandler : Panel.IHandler, Container.IHandler, Control.IHandler, Widget.IHandler, IContextMenuHost
        {
            bool SupportsCreateGraphics {
                get;
            }

            bool CanFocus {
                get;
                set;
            }

            void Create();

            void Create(bool largeCanvas);

            void Update(Rectangle region);

            Graphics CreateGraphics();
        }

        private static readonly object callback = new CustomPanel.Callback();

        [method: CompilerGenerated]
        [CompilerGenerated]
        public event EventHandler<PaintEventArgs> Paint;

        private new CustomPanel.IHandler Handler {
            get {
                return (CustomPanel.IHandler)base.Handler;
            }
        }

        public bool SupportsCreateGraphics {
            get {
                return Handler.SupportsCreateGraphics;
            }
        }

        public bool CanFocus {
            get {
                return Handler.CanFocus;
            }
            set {
                Handler.CanFocus = value;
            }
        }

        public CustomPanel()
        {
            Handler.Create();
            base.Initialize();
        }

        /*protected CustomPanel(CustomPanel.IHandler handler) : base(handler)
        {
        }*/

        public CustomPanel(bool largeCanvas)
        {
            Handler.Create(largeCanvas);
            base.Initialize();
        }

        protected virtual void OnPaint(PaintEventArgs e)
        {
            if (Paint != null) {
                Paint(this, e);
            }
        }

        public Graphics CreateGraphics()
        {
            return Handler.CreateGraphics();
        }

        public void Update(Rectangle region)
        {
            Handler.Update(region);
        }

        protected override object GetCallback()
        {
            return CustomPanel.callback;
        }
    }
}
