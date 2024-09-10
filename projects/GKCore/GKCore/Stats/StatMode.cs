/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2024 by Sergey V. Zhdanovskih.
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
    /// Types of statistics.
    /// </summary>
    public enum StatsMode
    {
        smAncestors,
        smDescendants,
        smDescGenerations,
        smSurnames,
        smNames,
        smPatronymics,
        smAge,
        smLifeExpectancy,
        smBirthYears,
        smBirthTenYears,
        smDeathYears,
        smDeathTenYears,
        smChildsCount,
        smChildsDistribution,
        smBirthPlaces,
        smDeathPlaces,
        smResidences,
        smOccupation,
        smReligious,
        smNational,
        smEducation,
        smCaste,
        smFirstbornAge,
        smMarriages,
        smMarriageAge,
        smSpousesDiff,
        smHobby,
        smAward,
        smMili,
        smMiliInd,
        smMiliDis,
        smMiliRank,
        smAAF_1,
        smAAF_2,
        smCertaintyIndex,
        smBirthByMonth,
        smDemography,
        smParentsAge,
        
        smLast = smParentsAge
    }
}
