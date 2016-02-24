namespace GKCore.Options
{
    public sealed class LangRecord
    {
        public readonly ushort Code;
        public readonly string Name;
        public readonly string FileName;

        public LangRecord(ushort code, string name, string fileName)
        {
            this.Code = code;
            this.Name = name;
            this.FileName = fileName;
        }
    }
}