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
using GKCommon.IoC;
using GKUI.Charts;

namespace GKUI.Engine
{
    /// <summary>
    /// This class implements initialization of IoC-container for WinForms presentation.
    /// </summary>
    public static class WinFormsBootstrapper
    {
        public static void Configure(IContainer container)
        {
            if (container == null)
                throw new ArgumentNullException("container");

            container.Register<IStdDialogs, WinFormsStdDialogs>(LifeCycle.Singleton);
            container.Register<IUIHelper, UIHelper>(LifeCycle.Singleton);
            container.Register<IUtilities, Utilities>(LifeCycle.Singleton);

            // controls and other
            container.Register<ITreeChartBox, TreeChartBox>(LifeCycle.Transient);
            //container.Register<IWizardPages, WizardPages>(LifeCycle.Transient);
        }
    }
}
