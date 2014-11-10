/* CFilenameAndSize.cs
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

namespace GEDmill.HTMLClasses
{
  // Once a file has been copied to the output directory, it needn't be copied again. 
  // However, we still need to know the size of the image, even if we don't copy it a 
  // second time. This class encapsulates the size along with the sFilename.
  public class CFilenameAndSize
  {
    public string m_sFilename;
    public int m_nWidth;
    public int m_nHeight;

    // Noddy constructor
    public CFilenameAndSize( string filename, int width, int height )
    {
      m_sFilename = filename;
      m_nWidth = width;
      m_nHeight = height;
    }
  };

}
