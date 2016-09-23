using System;
using System.Text;

namespace Sandbox
{
    /// <summary>
    /// Род
    /// </summary>
    public enum DeclensionGender
    {
        /// <summary>
        /// Род неопределен
        /// </summary>
        None = 0,

        /// <summary>
        /// Мужской род
        /// </summary>
        Masculine = 1,

        /// <summary>
        /// Женский род
        /// </summary>
        Feminine = 2,

        /// <summary>
        /// Род неопределен
        /// </summary>
        NotDefind = 3
    }

    /// <summary>
    /// Падежи русского языка
    /// </summary>
    public enum DeclensionCase
    {
        /// <summary>
        /// Падеж не определен
        /// </summary>
        None = 0,

        /// <summary>
        /// Именительный падеж (Кто? Что?)
        /// </summary>
        Nominative = 1,

        /// <summary>
        /// Родительный падеж (Кого? Чего?)
        /// </summary>
        Genitive = 2,

        /// <summary>
        /// Дательный падеж (Кому? Чему?)
        /// </summary>
        Dative = 3,

        /// <summary>
        /// Винительный падеж (Кого? Что?)
        /// </summary>
        Accusative = 4,

        /// <summary>
        /// Творительный падеж (Кем? Чем?)
        /// </summary>
        Instrumental = 5,

        /// <summary>
        /// Предложный падеж (О ком? О чём?)
        /// </summary>
        Prepositional = 6
    }

    // term checked
    public enum PersonalPronoun
    {
        Surname, Name, Patronymic
    }

    // Алгоритм: "Крохотулька"
    public static class RusDeclension
    {
        /// <summary>
        /// Функция для склонения одного слова!!!
        /// </summary>
        /// <param name="word">само слово</param>
        /// <param name="declCase">номер падежа </param>
        /// <param name="gender">пол</param>
        /// <param name="part">1-склонять как фамилию, 2-имя, 3-отчество</param>
        /// <returns></returns>
        private static string _declension(string word, int declCase /*=2*/, string gender /* = "*" */, int part /* =0 */)
        {
            if (word.Length == 2 && part == 1)
                return word;

            int z5 = InStr(word, "-");
            string z6 = string.Empty;

            if (z5 > 0) {
                string[] _name = word.Split('-');
                if (part == 1) {
                    z6 = "-" + _declension(_name[1], declCase, gender, part);
                    word = LCase(_name[0]);
                } else {
                    word = LCase(_name[0]) + "-" + UCase(Left(_name[1], 1)) + Mid(_name[1], 2);
                }
            } else {
                word = LCase(word);
            }

            string res;
            DeclensionCase _dc = (DeclensionCase)Max(declCase, -declCase);

            string z7 = Right(word, 3);
            string z8 = Right(z7, 2);
            string z9 = Right(z8, 1);
            z5 = word.Length;

            // ая , ия, ел = орел, ок = брелок на = весна, ма = зима
            int za = InStr("ая ия ел ок яц ий па да ца ша ба та га ка на ма", z8);
            int zb = InStr("аеёийоуэюяжнгхкчшщ", Left(z7, 1));
            int zd = (za == 4) ? 5 : InStr("айяь", z9);

            if (_dc == DeclensionCase.Nominative || z9 == "." ||
                (part == 2 && InStr("оиеу" + (gender == "ч" ? "" : "бвгджзклмнпрстфхцчшщъ"), z9) > 0) ||
                (part == 1 && InStr("мия мяэ лия кия жая лея", z7) > 0) ||
                (part < 3 && InStr("эы", z9) > 0)) {
                zd = 9;
            } else if (zd == 4 && gender == "ч") {
                zd = 2;
            } else if (part == 1) {
                if (InStr("оеиую", z9) + InStr("их ых аа еа ёа иа оа уа ыа эа юа яа", z8) > 0) {
                    zd = 9;
                } else if (gender != "ч") {
                    if (za == 1) {
                        zd = 7;
                    } else if (z9 == "а") {
                        zd = (za > 18 && z8 != "на") ? 1 : 6;
                    } else {
                        zd = 9;
                    }
                } else if (((InStr("ой ый", z8) > 0) && (z5 > 4) && (InStr("опой вбой", Right(word, 4)) == 0)) || (zb > 10 && za == 16)) {
                    zd = 8;
                }
            }

            int ze = InStr("лец нёк вей бей дец пец мец нец рец вец бец тец жец аец иец ыец бер", z7);

            if (zd == 8 && _dc != DeclensionCase.Instrumental) {
                res = (zb > 15 || InStr("жий ний", z7) > 0) ? "е" : "о";
            } else if (word == "лев") {
                res = "ьв";
            } else {
                if (InStr("аеёийоуыэюя", Mid(word, z5 - 3, 1)) == 0 && (zb > 11 || zb == 0) && ze != 61) res = "";
                else if (za == 7) res = "л";
                else if (za == 10) res = "к";
                else if (za == 13) res = "йц";
                else if (ze == 0) res = "";
                else if (ze == 13) res = "е";
                else if (ze < 16) res = "ь" + (ze == 1 ? "ц" : (ze == 5 ? "к" : ""));
                else if (ze < 53) res = "ц";
                else if (ze < 65) res = "йц";
                else res = "р";
            }

            if (zd == 9 || ((part == 3) && (Right(word, 1) == "ы"))) {
                res = word;
            } else {
                StringBuilder temp = new StringBuilder(128);
                temp.Append("а у а ");
                if ((z8 == "ич" && !fomitch(word)) || (z8 == "ыш"))
                    temp.Append("е");
                else if ((z8 == "ов") || (z8 == "ев" && part == 1) || (z8 == "ин" && part != 2))
                    temp.Append("ы");
                else
                    temp.Append("о");

                temp.Append("ме ");
                temp.Append(InStr("гжкхш", Left(z8, 1)) > 0 ? "и" : "ы");
                temp.Append(" е у ");
                temp.Append(z8 == "ца" ? "е" : "о");
                temp.Append("йе я ю я ем");
                temp.Append(za == 16 ? "и" : "е");
                temp.Append(" и е ю ейе и и ь ьюи и и ю ейи ойойу ойойойойуюойойгомуго");
                temp.Append((res == "е") || (za == 16) || ((zb > 12) && (zb < 16)) ? "и" : "ы");
                temp.Append("мм");

                string template = temp.ToString();

                res = Left(word, z5 - ((zd > 6) || (!string.IsNullOrEmpty(res)) ? 2 : (zd > 0 ? 1 : 0))) + res +
                    RTrim(Mid(template, 10 * zd + 2 * (int)_dc - 3, 2));
            }

            return (string.IsNullOrEmpty(word) ? "" : (part > 0 ? UCase(Left(res, 1)) + ((declCase < 0) && (part > 1) ? "." : Mid(res, 2)) : res) + z6);
        }

        private static bool fomitch(string familia)
        {
            string[] fomitchi = new string[] { "ильич", "кузьмич", "фомич", "лукич" };

            foreach (string st in fomitchi) {
                if (st == familia) return true;
            }

            return false;
        }

        //_____________________________________________________________________________
        // z1 - фамилия имя отчество например Железняков Юрий Юрьевич
        // z2 - Падеж ( по  умолчанию = 2 - родительный)
        // 2 - родительный  ( нет кого?    ) Железнякова Юрия Юрьевича
        // 3 - дательный    ( кому?        ) Железнякову Юрию Юрьевичу
        // 4 - винительный  ( вижу кого?   ) Железнякова Юрия Юрьевича
        // 5 - творительный ( кем?         ) Железняковым Юрием Юрьевичем
        // 6 - предложный   ( о ком?       ) Железнякове Юрии Юрьевиче
        // Если задать Z2 меньше 0, то на выходе получим от -1=Железняков Ю. Ю. до -6=Железнякове Ю. Ю.
        // z3 - параметр Пол может не указываться, но при наличии фамилий с
        // инициалами точное определение пола невозможно, поэтому предлагается задавать пол этим
        // параметром  1 - мужской 2 - женский
        // ---------------------------------------------------------------------------------------
        // Бибик Галушка Цой Николайчик Наталия Петровна Герценберг Кривошей Капица-Метелица
        // Если Падеж(Фио ,1 ,3),       то на выходе получим Фамилия Имя Отчество и т.д.
        // Если Падеж(Фио ,1 ,3,"1" ),  то                   Фамилия
        // Если Падеж(Фио ,1 ,3,"2" ),  то                   Имя
        // Если Падеж(Фио ,1 ,3,"3" ),  то                   Отчество
        // Если Падеж(Фио, 1 ,3,"12" ), то                   Фамилия Имя
        // Если Падеж(Фио, 1 ,3,"23" ), то                   Имя Отчество
        // Если Падеж(Фио,-1 ,3,"231" ),то                   И. О. Фамилия
        // Если Падеж(Фио,-1 ,3,"23" ), то                   И. О.
        // 10-11-2003 3-20

        /// <summary>
        /// 
        /// </summary>
        /// <param name="fn">фамилия имя отчество например Железняков Юрий Юрьевич</param>
        /// <param name="declansionCase">Падеж</param>
        /// <param name="gender">параметр Пол может не указываться, но при наличии фамилий с
        /// инициалами точное определение пола невозможно, поэтому предлагается задавать пол этим
        /// параметром  1 - мужской 2 - женский  </param>
        /// <param name="part"></param>
        /// <param name="z5"></param>
        /// <returns></returns>
        public static string GetDeclension(string fn, DeclensionCase declCase /*=2*/, DeclensionGender gender /*=3*/, /*Знач*/ string part /*="123"*/, int z5 /*=1*/)
        {
            string z6 = LCase(Right(RTrim(fn), 4));
            string z7 = Right(z6, 1);
            if (z5 < 4) {
                string _FIO = Trim(Replace(Mid(fn, InStr(fn + " ", " ") + 1), ".", ". "));
                string _base = ((z5 == 3) && (z7 == "ы") ? fn : Left(fn, InStr(fn + " ", " ") - 1));
                string _token = _declension(_base, (int)declCase, Mid("ча" + z7, (gender == DeclensionGender.NotDefind ? (z6 == "оглы" || z6 == "кызы" ? 1 : 3) : (int)gender), 1), z5) + " ";
                string _part = Replace(part, z5.ToString(), _token);

                return GetDeclension(_FIO, declCase, gender, _part, z5 + 1);
            } else {
                return part;
            }
        }

        /// <summary>
        /// 
        /// </summary>
        /// <param name="fn">full name</param>
        /// <param name="declCase">declension case</param>
        /// <returns></returns>
        public static string GetDeclension(string fn, DeclensionCase declCase)
        {
            try
            {
                return GetDeclension(fn, declCase, DeclensionGender.NotDefind, "123", 1);
            }
            catch
            {
                return "(ошибка)";
            }
        }

        #region Aux functions

        private static string Trim(string s)
        {
            return s.Trim();
        }

        private static string Replace(string p, string p_2, string p_3)
        {
            return p.Replace(p_2, p_3);
        }

        private static int InStr(string s, string match)
        {
            return s.IndexOf(match) + 1;
        }

        static int MinMax(int x, int min, int max)
        {
            return Math.Max(Math.Min(x, max), min);
        }

        private static string Mid(string s, int i, int len)
        {
            int start = MinMax(i - 1, 0, s.Length);
            return s.Substring(start, MinMax(len, 0, s.Length - start));
        }

        private static string Mid(string s, int i)
        {
            if (i > s.Length) return string.Empty;

            return s.Substring(i - 1);
        }

        private static string Left(string s, int len)
        {
            return s.Substring(0, Math.Min(len, s.Length));
        }

        private static string Right(string s, int len)
        {
            return s.Substring(s.Length - Math.Min(len, s.Length));
        }

        private static string LCase(string s)
        {
            return s.ToLowerInvariant();
        }

        private static string UCase(string p)
        {
            return p.ToUpperInvariant();
        }

        private static int Max(int x, int y)
        {
            return x > y ? x : y;
        }

        private static string RTrim(string s)
        {
            return s.TrimEnd();
        }

        #endregion
    }
}
