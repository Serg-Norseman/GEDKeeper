/* CPaintbox.cs
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
using System.Drawing.Imaging;

namespace GEDmill.ImageClasses
{
  // A data structure to contain the graphics drawing elements in the appropriate colours for the tree diagram.
  public class CPaintbox
  {
    public SolidBrush m_brushBg;
    public SolidBrush m_brushBox;
    public SolidBrush m_brushBoxHighlight;
    public SolidBrush m_brushBoxConcealed;
    public SolidBrush m_brushBoxShade;
    public SolidBrush m_brushText;
    public SolidBrush m_brushTextLink;
    public SolidBrush m_brushTextConcealed;
    public Pen m_penConnector;
    public Pen m_penConnectorDotted;
    public Pen m_penBox;
    public Font m_font;
    public ColorPalette m_palette;
    public TextureBrush m_brushFakeTransparency;
    public SolidBrush m_brushBgGif;
    public Color m_colourOutline;
    public Color m_colourBox;
    public Color m_colourHighlight;
    public Color m_colourBgConcealed;
    public Color m_colourShade;
    public Color m_colourText;
    public Color m_colourLink;
    public Color m_colourFgConcealed;
    public Color m_colourConnector;
    public Color m_colourGifTransparent;
    public Color m_colourBg;

    // Construct the paintbox, reading values for the colours etc from the config.
    public CPaintbox( CConfig config )
    {   
      m_colourBg = ConvertColour( config.m_sMiniTreeColourBackground );
      m_colourOutline = ConvertColour( config.m_sMiniTreeColourIndiBorder );
      m_colourBox = ConvertColour( config.m_sMiniTreeColourIndiBackground );
      m_colourHighlight = ConvertColour( config.m_sMiniTreeColourIndiHighlight );
      m_colourBgConcealed = ConvertColour( config.m_sMiniTreeColourIndiBgConcealed );
      m_colourShade = ConvertColour( config.m_sMiniTreeColourIndiShade );
      m_colourText = ConvertColour( config.m_sMiniTreeColourIndiText );
      m_colourLink = ConvertColour( config.m_sMiniTreeColourIndiLink );
      m_colourFgConcealed = ConvertColour( config.m_sMiniTreeColourIndiFgConcealed );
      m_colourConnector = ConvertColour( config.m_sMiniTreeColourBranch );
      m_colourGifTransparent = Color.Magenta;
      m_brushBgGif = new SolidBrush( m_colourGifTransparent );
      m_brushBg = new SolidBrush( m_colourBg );
      m_brushBox = new SolidBrush( m_colourBox );
      m_brushBoxHighlight = new SolidBrush( m_colourHighlight );
      m_brushBoxConcealed = new SolidBrush( m_colourBgConcealed );
      m_brushBoxShade = new SolidBrush( m_colourShade );
      m_brushText = new SolidBrush( m_colourText );
      m_brushTextLink = new SolidBrush( m_colourLink );
      m_brushTextConcealed = new SolidBrush( m_colourFgConcealed );
      m_penConnector = new Pen( m_colourConnector, 1.0f );
      m_penConnectorDotted = new Pen( m_colourConnector, 1.0f );
      m_penConnectorDotted.DashStyle = System.Drawing.Drawing2D.DashStyle.Dot;
      m_penBox = new Pen( m_colourOutline, 1.0f );
      m_font = new Font( "Microsoft Sans Serif", 10f );
      m_brushFakeTransparency = null;
      m_font = new Font( config.m_sTreeFontName, config.m_fTreeFontSize );
      m_brushFakeTransparency = null;
    }

    // Converts a string of the form #RRGGBB to a Color instance.
    // Used when retrieving colours from the config.
    public static Color ConvertColour( string s )
    {
      if( s == null || s == "" )
      {
        return Color.Black;
      }

      int nRed = 0;
      int nGreen = 0;
      int nBlue = 0;

      switch( s.Length )
      {
        case 4:
          s = s.Substring( 1 );
          goto case 3;
        case 3:
          nRed = System.Int32.Parse( s.Substring(0,1), System.Globalization.NumberStyles.HexNumber );
          nGreen = System.Int32.Parse( s.Substring(1,1), System.Globalization.NumberStyles.HexNumber );
          nBlue = System.Int32.Parse( s.Substring(2,1), System.Globalization.NumberStyles.HexNumber );
          break;
        case 7:
          s = s.Substring( 1 );
          goto case 6;
        case 6:
          nRed = System.Int32.Parse( s.Substring(0,2), System.Globalization.NumberStyles.HexNumber );
          nGreen = System.Int32.Parse( s.Substring(2,2), System.Globalization.NumberStyles.HexNumber );
          nBlue = System.Int32.Parse( s.Substring(4,2), System.Globalization.NumberStyles.HexNumber );
          break;
      }
      
      return Color.FromArgb( nRed, nGreen, nBlue );
    }
    
    // Converts a Color instance to a string of the form #RRGGBB.
    // Used when storing colours in the config.
    public static string ConvertColour( Color c )
    {
      string s = String.Format( "#{0:X2}{1:X2}{2:X2}", c.R, c.G, c.B );
      return s;
    }

    // Sets the brush used to fill the background to the graphics image provided.
    public void SetBackgroundImage(string filename)
    {
      if (filename != null && filename.Length > 0)
      {
        try
        {
          Image bgImage = Image.FromFile(filename);
          m_brushFakeTransparency = new TextureBrush(bgImage);
        }
        catch (Exception e) 
        {
          // e.g. System.IO.FileNotFoundException
          LogFile.TheLogFile.WriteLine(LogFile.DT_HTML, LogFile.EDebugLevel.eDL_Note, String.Format("SetBackgroundImage() Caught exception {0}", e.ToString()));
          m_brushFakeTransparency = null;
        }
      }
    }
  }
}
