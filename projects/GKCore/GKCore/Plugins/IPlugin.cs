/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
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
