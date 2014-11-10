/* CMiniTreeMap.cs
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

using GEDmill.LLClasses;

namespace GEDmill.ImageClasses
{
  // A data structure containing coordinates of a clickable area on the mini tree. 
  // Used to create HTML image alMap.
  public class CMiniTreeMap
  {
    // The name of the individual in the box. Used for ALT attribute.
    public string m_sName;

    // The left coordinate of the box.
    public int m_x1;

    // The top coordinate of the box.
    public int m_y1;

    // The right coordinate of the box.
    public int m_x2;

    // The bottom coordinate of the box.
    public int m_y2;

    // The individual record reached by clicking this box. Used for HREF.
    public CIndividualRecord m_ir;

    // Whether this individual should be a clickable region on the alMap.
    public bool m_bLinkable;

    // Constructor.
    public CMiniTreeMap( string sName, CIndividualRecord ir, bool bLinkable, int x1, int y1, int x2, int y2 )
    {
      m_x1 = x1;
      m_y1 = y1;
      m_x2 = x2;
      m_y2 = y2;
      m_ir = ir;
      m_bLinkable = bLinkable;
      m_sName = sName;
    }
  }
}
