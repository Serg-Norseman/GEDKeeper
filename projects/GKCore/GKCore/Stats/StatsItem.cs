/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
 */

namespace GKCore.Stats
{
    /// <summary>
    /// 
    /// </summary>
    public sealed class StatsItem
    {
        public string Caption;
        public int Value;

        public bool IsCombo;
        public int ValF;
        public int ValM;

        public StatsItem(string caption, bool isCombo)
        {
            Caption = caption;
            Value = 0;
            IsCombo = isCombo;
        }

        public StatsItem(string caption, int value)
        {
            Caption = caption;
            Value = value;
            IsCombo = false;
        }

        public override string ToString()
        {
            return Caption;
        }

        public string GetDisplayString()
        {
            string stVal = (!IsCombo) ? Value.ToString() : ValF.ToString() + " | " + ValM.ToString();
            return stVal;
        }
    }
}
