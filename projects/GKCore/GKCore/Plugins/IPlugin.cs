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

using System;
using BSLib;
using GKCore.Design.Graphics;
using GKCore.Locales;

namespace GKCore.Plugins
{
    public class HostClosingEventArgs : EventArgs
    {
        private bool fCancel;

        public bool Cancel
        {
            get { return fCancel; }
            set { fCancel = value; }
        }

        public HostClosingEventArgs()
        {
            fCancel = false;
        }
    }

    public enum PluginCategory
    {
        /// <summary>
        /// Plugin general purpose.
        /// </summary>
        Common,

        /// <summary>
        /// A plugin that modifies data, visual representation of data,
        /// or ancillary capabilities for working with data.
        /// </summary>
        Tool,

        /// <summary>
        /// Plugin for exporting data to external report files.
        /// </summary>
        Report,

        /// <summary>
        /// 
        /// </summary>
        DialogReplacement,

        /// <summary>
        /// 
        /// </summary>
        Background,
    }

    public interface IPlugin
    {
        /// <summary>
        /// The name of plugin, for display in the menu item.
        /// </summary>
        string DisplayName { get; }

        /// <summary>
        /// Pointer to the main form of application.
        /// </summary>
        IHost Host { get; }

        ILangMan LangMan { get; }

        IImage Icon { get; }

        PluginCategory Category { get; }

        void Execute();
        bool Startup(IHost host);
        bool Shutdown();

        void OnHostClosing(HostClosingEventArgs eventArgs);
        void OnHostActivate();
        void OnHostDeactivate();
        void OnLanguageChange();

        void LoadOptions(IniFile ini);
        void SaveOptions(IniFile ini);
    }
}
