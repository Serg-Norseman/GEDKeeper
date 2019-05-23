/* CPGQualifiedDate.cs
 * 
 * Copyright 2009 Alexander Curtis <alex@logicmill.com>
 * This file is part of GEDmill - A family history website creator
 * 
 * GEDmill is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * GEDmill is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with GEDmill.  If not, see <http://www.gnu.org/licenses/>.
 *
 *
 * History:  
 * 10Dec08 AlexC          Migrated from GEDmill 1.10
 *
 */

using GDModel;

namespace GEDmill.Model
{
    public enum DateQualification
    {
        Birth,
        Baptism,
        Christening,
        Death,
        Burial,
        Cremation
    }

    /// <summary>
    /// Class used to find the best date to use for an individual's birth or death.
    /// Naturally an actual birth record would be best, but if this is missing, we can use the Christening date etc..
    /// See also GDMDateValue.
    /// </summary>
    public class QualifiedDate
    {
        public DateQualification Qualification;
        public GDMDateValue Date;


        public QualifiedDate(GDMDateValue date, DateQualification qualification)
        {
            Date = date;
            Qualification = qualification;
        }

        // Just delegates to GDMDateValue::ToString()
        public override string ToString()
        {
            if (Date != null) {
                return Date.ToString();
            }
            return "";
        }
    }
}
