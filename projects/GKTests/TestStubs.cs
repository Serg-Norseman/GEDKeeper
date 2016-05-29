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

using GKCommon.GEDCOM;
using GKCore;

using NUnit.Framework;

namespace GKTests
{
    public static class TestStubs
    {
        public static BaseContext CreateContext()
        {
            GEDCOMTree tree = new GEDCOMTree();
            Assert.IsNotNull(tree);

            BaseContext context = new BaseContext(tree, null);
            return context;
        }

        public static void FillContext(BaseContext context)
        {
            // a null result if the record is not defined
            GEDCOMCustomEvent evt = context.CreateEventEx(null, "BIRT", "xxxxx", "xxxxx");
            Assert.IsNull(evt);

            // first individual
            GEDCOMIndividualRecord iRec = context.CreatePersonEx("Ivan", "Ivanovich", "Ivanov", GEDCOMSex.svMale, true);
            Assert.IsNotNull(iRec);

            evt = iRec.FindEvent("BIRT");
            Assert.IsNotNull(evt);
            evt.Detail.Date.ParseString("28 DEC 1990");
            evt.Detail.Place.StringValue = "Ivanovo";

            GEDCOMCustomEvent evtd = context.CreateEventEx(iRec, "DEAT", "28 DEC 2010", "Ivanovo");
            Assert.IsNotNull(evtd);

            // second individual, wife
            GEDCOMIndividualRecord iRec2 = context.CreatePersonEx("Maria", "Petrovna", "Ivanova", GEDCOMSex.svFemale, true);
            evt = iRec2.FindEvent("BIRT");
            Assert.IsNotNull(evt);
            evt.Detail.Date.ParseString("17 MAR 1990");
            evt.Detail.Place.StringValue = "Ivanovo";

            // third individual, child
            GEDCOMIndividualRecord iRec3 = context.CreatePersonEx("Anna", "Ivanovna", "Ivanova", GEDCOMSex.svFemale, true);
            evt = iRec3.FindEvent("BIRT");
            Assert.IsNotNull(evt);
            evt.Detail.Date.ParseString("11 FEB 2010");
            evt.Detail.Place.StringValue = "Ivanovo";

            // their family
            GEDCOMFamilyRecord famRec = context.Tree.CreateFamily();
            Assert.IsNotNull(famRec);
            famRec.AddSpouse(iRec);
            famRec.AddSpouse(iRec2);
            famRec.AddChild(iRec3);

            // individual outside the family
            GEDCOMIndividualRecord iRec4 = context.CreatePersonEx("Alex", "", "Petrov", GEDCOMSex.svMale, true);
            evt = iRec4.FindEvent("BIRT");
            Assert.IsNotNull(evt);
            evt.Detail.Date.ParseString("15 JUN 1989");
            evt.Detail.Place.StringValue = "Far Forest";

            evt = context.CreateEventEx(iRec4, "RESI", "12 FEB", "Far Forest");
            Assert.IsNotNull(evt);

            // group for tests
            GEDCOMGroupRecord groupRec = context.Tree.CreateGroup();
            Assert.IsNotNull(groupRec, "group1 != null");
        }
    }
}
