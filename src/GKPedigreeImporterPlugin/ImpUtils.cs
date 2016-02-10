using System;
using BSLib;

namespace GKPedigreeImporterPlugin
{
	/// <summary>
	/// Description of ImpUtils.
	/// </summary>
	public static class ImpUtils
	{
		private const string RomeChars = "IVXLCDM";

		private static bool IsRomeChar(char c)
		{
			return (RomeChars.IndexOf(c) >= 0);
		}

		public static bool IsRomeLine(string str)
		{
			if (string.IsNullOrEmpty(str)) {
				return false;
			}

			string rs = "";
			int i = 0;
			while (i < str.Length && IsRomeChar(str[i])) {
				rs += str[i];
				i++;
			}
			return (rs != "" && rs == str);
		}

		public static string DeleteBlanks(string str)
		{
			string result = str;

			if (!string.IsNullOrEmpty(result))
			{
				int i = 0;
				while (i < result.Length)
				{
					if (result[i] == ' ') {
						result = result.Remove(i, 1);
					} else {
						i++;
					}
				}
			}

			return result;
		}

		public static bool IsPersonLine_DAboville(string str, ref string p_id)
		{
			// "11.21.31.11."

			p_id = "";

			StringTokenizer strTok = new StringTokenizer(str);
			strTok.RecognizeDecimals = false;

			Token token, prev = null;
			do
			{
				token = strTok.Next();

				if (token.Kind == TokenKind.Symbol && token.Value == ".") {
					if (prev != null && prev.Kind == TokenKind.Number) {
						p_id += token.Value;
						prev = token;
					} else {
						break;
					}
				} else if (token.Kind == TokenKind.Number) {
					p_id += token.Value;
					prev = token;
				} else {
					break;
				}
			} while (token.Kind != TokenKind.EOF);

			// TODO: testing and transfer to parse!
			if ((prev == null || prev.Kind != TokenKind.Symbol || prev.Value != ".") || (token.Kind != TokenKind.WhiteSpace || token.Value != " "))
			{
				return false;
			}

			return true;
		}

		public static bool ParsePersonLine_DAboville(string str, out string persId, out string parentId, out string marNum,
		                                            out string extData, out int pos)
		{
			// "11.21.31.11."

			persId = "";
			parentId = "";
			marNum = "";
			extData = "";
			pos = 0;

			StringTokenizer strTok = new StringTokenizer(str);
			strTok.RecognizeDecimals = false;

			Token token, prev = null;
			do
			{
				token = strTok.Next();

				if (token.Kind == TokenKind.Symbol && token.Value == ".") {
					if (prev != null && prev.Kind == TokenKind.Number) {
						persId += token.Value;
						prev = token;
					} else {
						break;
					}
				} else if (token.Kind == TokenKind.Number) {
					persId += token.Value;
					prev = token;
				} else {
					break;
				}
			} while (token.Kind != TokenKind.EOF);

			if (token.Kind != TokenKind.WhiteSpace || token.Value != " ") {
				return false;
			}

			int idx = persId.LastIndexOf(".", persId.Length - 2);
			if (idx > 0) {
				parentId = persId.Substring(0, idx + 1);
			}

			pos = strTok.Position;

			return true;
		}

		public static bool IsPersonLine_Konovalov(string str, ref string p_id)
		{
			// "11-21/1 (test+2, test)."

			p_id = "";

			StringTokenizer strTok = new StringTokenizer(str);
			strTok.RecognizeDecimals = false;
			Token token;

			token = strTok.Next();
			if (token.Kind == TokenKind.Number) {
				p_id += token.Value;
			} else {
				return false;
			}

			token = strTok.Next();
			if (token.Kind == TokenKind.Symbol && token.Value == "-") {
				p_id += token.Value;

				token = strTok.Next();
				if (token.Kind == TokenKind.Number) {
					p_id += token.Value;
				} else {
					return false;
				}

				token = strTok.Next();
				if (token.Kind == TokenKind.Symbol && token.Value == "/") {
					p_id += token.Value;

					token = strTok.Next();
					if (token.Kind == TokenKind.Number || (token.Kind == TokenKind.Symbol && token.Value == "?")) {
						p_id += token.Value;
					} else {
						return false;
					}

					token = strTok.Next();
				}
			}

			if (token.Kind == TokenKind.WhiteSpace && token.Value == " ") {
				p_id += token.Value;
				token = strTok.Next();
			}

			if (token.Kind == TokenKind.Symbol && token.Value == "(") {
				p_id += token.Value;

				do {
					token = strTok.Next();
					if (token.Kind == TokenKind.EOL || token.Kind == TokenKind.EOF) {
						return false;
					}
					p_id += token.Value;
				} while (token.Kind != TokenKind.Symbol || token.Value != ")");

				token = strTok.Next();
			}

			if (token.Kind == TokenKind.Symbol && token.Value == ".") {
				p_id += token.Value;
			} else {
				return false;
			}

			token = strTok.Next();
			if (token.Kind != TokenKind.WhiteSpace || token.Value != " ") {
				return false;
			}

			return true;
		}

		public static bool ParsePersonLine_Konovalov(string str, out string persId, out string parentId, out string marNum,
		                                            out string extData, out int pos)
		{
			// "11-21/1 (test+2, test)."

			persId = "";
			parentId = "";
			marNum = "";
			extData = "";
			pos = 0;

			StringTokenizer strTok = new StringTokenizer(str);
			strTok.RecognizeDecimals = false;
			Token token;

			token = strTok.Next();
			if (token.Kind == TokenKind.Number) {
				persId = token.Value;
			} else {
				return false;
			}

			token = strTok.Next();
			if (token.Kind == TokenKind.Symbol && token.Value == "-") {
				//p_id += token.Value;

				token = strTok.Next();
				if (token.Kind == TokenKind.Number) {
					parentId = token.Value;
				} else {
					return false;
				}

				token = strTok.Next();
				if (token.Kind == TokenKind.Symbol && token.Value == "/") {
					//p_id += token.Value;

					token = strTok.Next();
					if (token.Kind == TokenKind.Number || (token.Kind == TokenKind.Symbol && token.Value == "?")) {
						marNum = token.Value;
					} else {
						return false;
					}

					token = strTok.Next();
				}
			}

			if (token.Kind == TokenKind.WhiteSpace && token.Value == " ") {
				//p_id += token.Value;
				token = strTok.Next();
			}

			if (token.Kind == TokenKind.Symbol && token.Value == "(") {
				extData += token.Value;

				do {
					token = strTok.Next();
					if (token.Kind == TokenKind.EOL || token.Kind == TokenKind.EOF) {
						return false;
					}
					extData += token.Value;
				} while (token.Kind != TokenKind.Symbol || token.Value != ")");

				token = strTok.Next();
			}

			if (token.Kind == TokenKind.Symbol && token.Value == ".") {
				//p_id += token.Value;
			} else {
				return false;
			}

			token = strTok.Next();
			if (token.Kind != TokenKind.WhiteSpace || token.Value != " ") {
				return false;
			}

			pos = strTok.Position;

			return true;
		}

		////////////////////

		public static bool ParseSpouseLine(string str, out string spouse, out int marrNum, out string extData, out int pos)
		{
			// "М/Ж[N]{": " | " - "}name"

			spouse = "";
			marrNum = 1;
			extData = "";
			pos = 0;

			StringTokenizer strTok = new StringTokenizer(str);
			strTok.IgnoreWhiteSpace = false;
			strTok.RecognizeDecimals = false;

			Token token;

			token = strTok.Next();
			if (token.Kind == TokenKind.Word && (token.Value == "М" || token.Value == "Ж")) {
				spouse = token.Value;
				token = strTok.Next();
			} else {
				return false;
			}

			if (token.Kind == TokenKind.Number) {
				marrNum = int.Parse(token.Value);
				token = strTok.Next();
			} else {
				marrNum = 1;
			}

			if (token.Kind == TokenKind.WhiteSpace && token.Value == " ") {
				token = strTok.Next();
			}

			if (token.Kind == TokenKind.Symbol && token.Value == "(") {
				extData += token.Value;

				do {
					token = strTok.Next();
					if (token.Kind == TokenKind.EOL || token.Kind == TokenKind.EOF) {
						return false;
					}
					extData += token.Value;
				} while (token.Kind != TokenKind.Symbol || token.Value != ")");

				token = strTok.Next();
			}

			if (token.Kind == TokenKind.WhiteSpace && token.Value == " ") {
				token = strTok.Next();
			}

			if (token.Kind == TokenKind.Symbol && (token.Value == ":" || token.Value == "-")) {
				token = strTok.Next();
			} else {
				return false;
			}

			pos = strTok.Position;

			return true;
		}
	}
}
