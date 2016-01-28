namespace GKCore.Types
{
	/// <summary>
	/// PGNode - it's node class for Patriarchs Graph.
	/// </summary>
    public sealed class PGNode
    {
        public string FamilyXRef;
        public PGNodeType Type;
        public int Size;

        public PGNode(string label, PGNodeType type)
        {
            this.FamilyXRef = label;
            this.Type = type;
            this.Size = 1;
        }

        public PGNode(string label, PGNodeType type, int size)
        {
            this.FamilyXRef = label;
            this.Type = type;
            this.Size = size;
        }
    }
}