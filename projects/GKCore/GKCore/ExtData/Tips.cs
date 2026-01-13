/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
 */

using System;
using System.IO;
using BSLib;
using GKCore.Utilities;

namespace GKCore.ExtData
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
                throw new ArgumentNullException(nameof(tipsList));

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
