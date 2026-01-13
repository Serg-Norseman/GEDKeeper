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
    public sealed class CommonStats
    {
        public int persons;
        public int persons_m;
        public int persons_f;
        public int lives;
        public int lives_m;
        public int lives_f;

        public readonly CompositeItem age;
        public readonly CompositeItem life;
        public readonly CompositeItem childs;
        public readonly CompositeItem fba;
        public readonly CompositeItem marr;
        public readonly CompositeItem mage;
        public readonly CompositeItem cIndex;
        
        public CommonStats()
        {
            persons = 0;
            persons_m = 0;
            persons_f = 0;
            lives = 0;
            lives_m = 0;
            lives_f = 0;

            age = new CompositeItem();
            life = new CompositeItem();
            childs = new CompositeItem();
            fba = new CompositeItem();
            marr = new CompositeItem();
            mage = new CompositeItem();
            cIndex = new CompositeItem();
        }
    }
}
