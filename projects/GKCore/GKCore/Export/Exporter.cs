/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
 */

using System;
using BSLib;
using GDModel;
using GKCore.Design;
using GKCore.Export.Formats;
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
        protected GDMTree fTree;
        protected CustomWriter fWriter;

        public GlobalOptions Options
        {
            get { return fOptions; }
            set { fOptions = value; }
        }

        protected Exporter(IBaseWindow baseWin)
        {
            if (baseWin == null)
                throw new ArgumentNullException(nameof(baseWin));

            fBase = baseWin;
            fTree = baseWin.Context.Tree;
        }

        protected override void Dispose(bool disposing)
        {
            if (disposing) {
                if (fWriter != null) fWriter.Dispose();
            }
            base.Dispose(disposing);
        }

        public abstract void Generate(bool show);

        protected void ShowResult()
        {
            if (AppHost.TEST_MODE) return;

            GKUtils.LoadExtFile(fPath);
        }
    }
}
