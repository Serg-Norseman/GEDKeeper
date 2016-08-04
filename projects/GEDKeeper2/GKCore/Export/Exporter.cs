/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2016 by Serg V. Zhdanovskih (aka Alchemist, aka Norseman).
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

using GKCommon;
using GKCommon.GEDCOM;
using GKCore.Interfaces;
using GKCore.Options;

namespace GKCore.Export
{
    /// <summary>
    /// 
    /// </summary>
    public abstract class Exporter : BaseObject
    {
        protected readonly IBaseWindow fBase;
        protected GlobalOptions fOptions;
        protected string fPath;
        protected GEDCOMTree fTree;
        protected CustomWriter fWriter;

        public GlobalOptions Options
        {
            get { return this.fOptions; }
            set { this.fOptions = value; }
        }

        protected Exporter(IBaseWindow baseWin)
        {
            if (baseWin == null)
                throw new ArgumentNullException("baseWin");

            this.fBase = baseWin;
            this.fTree = baseWin.Tree;

            //if (!Directory.Exists(this.FPath)) Directory.CreateDirectory(this.FPath);
        }

        public abstract void Generate(bool show);

        protected void ShowResult()
        {
            GKUtils.LoadExtFile(this.fPath);
        }
    }
}
