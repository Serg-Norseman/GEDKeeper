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
