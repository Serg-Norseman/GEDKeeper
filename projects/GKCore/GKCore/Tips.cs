/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2017-2022 by Sergey V. Zhdanovskih.
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
using System.IO;
using BSLib;
using GKCore.Utilities;

namespace GKCore
{
    public sealed class Tip
    {
        public string Text;
    }

    internal class TipsList
    {
        public Tip[] Tips { get; set; }

        public TipsList()
        {
            Tips = new Tip[0];
        }
    }

    /// <summary>
    /// 
    /// </summary>
    public sealed class Tips
    {
        private TipsList fTips;

        public Tips()
        {
            fTips = new TipsList();
        }

        public void Load(string fileName)
        {
            if (!File.Exists(fileName))
                return;

            try {
                using (var reader = new StreamReader(fileName)) {
                    string content = reader.ReadToEnd();
                    fTips = YamlHelper.Deserialize<TipsList>(content);
                }

                fTips.Tips.Shuffle();
            } catch (Exception ex) {
                Logger.WriteError("Tips.Load()", ex);
            }
        }

        public void CollectTips(StringList tipsList)
        {
            if (tipsList == null)
                throw new ArgumentNullException("tipsList");

            try {
                for (int i = 0; i < fTips.Tips.Length; i++) {
                    tipsList.Add(fTips.Tips[i].Text);
                }
            } catch (Exception ex) {
                Logger.WriteError("Tips.CollectTips()", ex);
            }
        }
    }
}
