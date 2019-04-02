/* CAsidPair.cs
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

using System;
using System.Drawing;

namespace GEDmill.Model
{
    // An ASID pair is used by Family Historian to delimit an sArea of a larger picture that contains the 
    // individual in question.
    /*public class CAsidPair
    {
        // The ASID
        public string m_sAsid;
        
        // The sArea within the image
        public Rectangle m_rectArea;

        // Parsing constructor
        public CAsidPair( string sAsid, string sArea )
        {
            // sArea is modified so keep value passed in to display in error message at the end.
            string sOriginalArea = sArea;

            m_sAsid = sAsid;
            
            // Parse sArea string
            if (sArea == null || sArea == "")
            {
                return;
            }

            int nLeft,nTop,nBottom,nRight;
            try 
            {
                GEDCOMTree.ParseWhitespace( ref sArea );
                if (sArea[0] != '{')
                {
                    return;
                }

                sArea = sArea.Substring(1);
                nTop = GEDCOMTree.ParseNumber( ref sArea );
                if (nTop < 0)
                {
                    nTop = 0;
                }
                if (sArea[0] != ',')
                {
                    return;
                }

                sArea = sArea.Substring(1);
                nLeft = GEDCOMTree.ParseNumber( ref sArea );
                if (nLeft < 0)
                {
                    nLeft = 0;
                }
                if (sArea[0] != ',')
                {
                    return;
                }

                sArea = sArea.Substring(1);
                nBottom = GEDCOMTree.ParseNumber( ref sArea );
                if (nBottom < 0)
                {
                    nBottom = 0;
                }
                if (sArea[0] != ',')
                {
                    return;
                }

                sArea = sArea.Substring(1);
                nRight = GEDCOMTree.ParseNumber( ref sArea );
                if (nRight < 0)
                {
                    nRight = 0;
                }
                if (sArea[0] != '}')
                {
                    return;
                }
            }
            catch( CParsingException )
            {
                LogFile.TheLogFile.WriteLine( LogFile.DT_GEDCOM, LogFile.EDebugLevel.Error, string.Format("Caught CAsidPair exception {0}", sOriginalArea) );
                return;
            }
            m_rectArea = new Rectangle( nLeft, nTop, nRight-nLeft, nBottom-nTop );
        }

        // Copy constructor
        public CAsidPair( CAsidPair ap )
        {
            m_sAsid = ap.m_sAsid;
            m_rectArea = ap.m_rectArea;
        }

    }*/
}
