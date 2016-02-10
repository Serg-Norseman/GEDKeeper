using System.Windows.Forms;

using BSLib;
using GKCommon;

namespace GKCore.Options
{
    public class MRUFile
    {
        public string FileName;
        public ExtRect WinRect;
        public FormWindowState WinState;

        public void Load(IniFile iniFile, string section)
        {
            if (iniFile == null) return;

            this.FileName = iniFile.ReadString(section, "FileName", "");
            this.WinRect.Left = iniFile.ReadInteger(section, "WinL", 10);
            this.WinRect.Top = iniFile.ReadInteger(section, "WinT", 10);
            this.WinRect.Right = iniFile.ReadInteger(section, "WinR", 778);
            this.WinRect.Bottom = iniFile.ReadInteger(section, "WinB", 312);
            this.WinState = (FormWindowState)((uint)iniFile.ReadInteger(section, "WinState", 0));
        }

        public void Save(IniFile iniFile, string section)
        {
            if (iniFile == null) return;

            iniFile.WriteString(section, "FileName", this.FileName);
            iniFile.WriteInteger(section, "WinL", this.WinRect.Left);
            iniFile.WriteInteger(section, "WinT", this.WinRect.Top);
            iniFile.WriteInteger(section, "WinR", this.WinRect.Right);
            iniFile.WriteInteger(section, "WinB", this.WinRect.Bottom);
            iniFile.WriteInteger(section, "WinState", (int)this.WinState);
        }

        public static void DeleteKeys(IniFile iniFile, string section)
        {
            if (iniFile == null) return;

            iniFile.DeleteKey(section, "FileName");
            iniFile.DeleteKey(section, "WinL");
            iniFile.DeleteKey(section, "WinT");
            iniFile.DeleteKey(section, "WinR");
            iniFile.DeleteKey(section, "WinB");
            iniFile.DeleteKey(section, "WinState");
        }
    }
}