﻿/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2019 by Sergey V. Zhdanovskih.
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
using BSLib.Design.MVP;
using Eto.Forms;
using GKCore.Interfaces;
using GKUI.Components;

namespace GKUI.Forms
{
    /// <summary>
    /// 
    /// </summary>
    public class CommonForm : Form, IView
    {
        private readonly ControlsManager fControlsManager;

        #region View Interface

        public string Caption
        {
            get { return base.Title; }
            set { base.Title = value; }
        }

        #endregion

        public CommonForm()
        {
            fControlsManager = new ControlsManager();
        }

        public void SetToolTip(BindableWidget component, string toolTip)
        {
            if (component != null && !string.IsNullOrEmpty(toolTip)) {
                if (component is Control) {
                    ((Control)component).ToolTip = toolTip;
                } else
                if (component is ToolItem) {
                    ((ToolItem)component).ToolTip = toolTip;
                }
            }
        }

        public void Activate()
        {
            Focus();
        }

        protected T GetControlHandler<T>(object control) where T : class, IControl
        {
            return fControlsManager.GetControlHandler<T>(control);
        }
    }


    /// <summary>
    /// 
    /// </summary>
    public abstract class CommonWindow : CommonForm, IWindow
    {
        public virtual void Show(bool showInTaskbar)
        {
            ShowInTaskbar = showInTaskbar;
            Show();
        }

        public virtual void SetLang()
        {
        }
    }


    /// <summary>
    /// 
    /// </summary>
    public class CommonDialog : Dialog<DialogResult>, ICommonDialog
    {
        private readonly ControlsManager fControlsManager;

        #region View Interface

        public string Caption
        {
            get { return base.Title; }
            set { base.Title = value; }
        }

        #endregion

        public DialogResult DialogResult
        {
            get { return base.Result; }
            set {
                if (base.Result != value) {
                    base.Result = value;
                    if (value != DialogResult.None) {
                        Close();
                    }
                }
            }
        }

        public CommonDialog() : base()
        {
            Maximizable = false;
            Minimizable = false;
            Resizable = false;
            ShowInTaskbar = false;

            fControlsManager = new ControlsManager();
        }

        public void SetToolTip(BindableWidget component, string toolTip)
        {
            if (component != null && !string.IsNullOrEmpty(toolTip)) {
                if (component is Control) {
                    ((Control)component).ToolTip = toolTip;
                } else
                if (component is ToolItem) {
                    ((ToolItem)component).ToolTip = toolTip;
                }
            }
        }

        public void Activate()
        {
            Focus();
        }

        public virtual bool ShowModalX(object owner)
        {
            return (ShowModal((Control)owner) == DialogResult.Ok);
        }

        protected virtual void CancelClickHandler(object sender, EventArgs e)
        {
            Close(DialogResult.Cancel);
        }

        public void SetPredefProperties(int width, int height, bool fontPreset = true)
        {
            UIHelper.SetPredefProperties(this, width, height, fontPreset);
        }

        protected T GetControlHandler<T>(object control) where T : class, IControl
        {
            return fControlsManager.GetControlHandler<T>(control);
        }
    }
}
