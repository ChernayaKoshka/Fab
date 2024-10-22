Turns this [(credit)](https://en.wikipedia.org/wiki/Augmented_Backus–Naur_form):
```
postal-address   = name-part street zip-part

name-part        = *(personal-part SP) last-name [SP suffix] CRLF
name-part        =/ personal-part CRLF

personal-part    = first-name / (initial ".")
first-name       = *ALPHA
initial          = ALPHA
last-name        = *ALPHA
suffix           = ("Jr." / "Sr." / 1*("I" / "V" / "X"))

street           = [apt SP] house-num SP street-name CRLF
apt              = 1*4DIGIT
house-num        = 1*8(DIGIT / ALPHA)
street-name      = 1*VCHAR

zip-part         = town-name "," SP state 1*2SP zip-code CRLF
town-name        = 1*(ALPHA / SP)
state            = 2ALPHA
zip-code         = 5DIGIT ["-" 4DIGIT]
```

Into this:
| Rule Name      | Resulting Regular Expression
|----------------|------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
| apt | ^\(?:\[0\-9\]\{1,4\}?)$ |
| first-name | ^\(?:\[A\-za\-z\]\*?)$ |
| house-num | ^\(?:\(?:\[0\-9\]\|\[A\-za\-z\])\{1,8\}?)$ |
| initial | ^\(?:\[A\-za\-z\])$ |
| last-name | ^\(?:\[A\-za\-z\]\*?)$ |
| name-part | ^\(?:\(?:\(?'personal\_part'\(?:\(?'first\_name'\[A\-za\-z\]\*?)\|\(?:\(?'initial'\[A\-za\-z\])\\\.)))\\r\\n)\|\(?:\(?:\(?'personal\_part'\(?:\(?'first\_name'\[A\-za\-z\]\*?)\|\(?:\(?'initial'\[A\-za\-z\])\\\.)))\\ )\*?\(?'last\_name'\[A\-za\-z\]\*?)\(?:\(?:\\ \(?'suffix'\(?:\(?i)Jr\\\.\(?\-i)\|\(?i)Sr\\\.\(?\-i)\|\(?:\(?i)I\(?\-i)\|\(?i)V\(?\-i)\|\(?i)X\(?\-i))\+?))))?\\r\\n))$ |
| personal-part | ^\(?:\(?:\(?'first\_name'\[A\-za\-z\]\*?)\|\(?:\(?'initial'\[A\-za\-z\])\\\.)))$ |
| postal-address | ^\(?:\(?:\(?'name\_part'\(?:\(?:\(?:\(?:\(?'personal\_part'\(?:\(?'first\_name'\[A\-za\-z\]\*?)\|\(?:\(?'initial'\[A\-za\-z\])\\\.)))\\ )\*?\(?'last\_name'\[A\-za\-z\]\*?)\(?:\(?:\\ \(?'suffix'\(?:\(?i)Jr\\\.\(?\-i)\|\(?i)Sr\\\.\(?\-i)\|\(?:\(?i)I\(?\-i)\|\(?i)V\(?\-i)\|\(?i)X\(?\-i))\+?))))?\\r\\n))\|\(?:\(?:\(?'personal\_part'\(?:\(?'first\_name'\[A\-za\-z\]\*?)\|\(?:\(?'initial'\[A\-za\-z\])\\\.)))\\r\\n))))\(?'street'\(?:\(?:\(?:\(?'apt'\[0\-9\]\{1,4\}?)\\ ))?\(?'house\_num'\(?:\[0\-9\]\|\[A\-za\-z\])\{1,8\}?)\\ \(?'street\_name'\[\!\-~\]\+?)\\r\\n))\(?'zip\_part'\(?:\(?'town\_name'\(?:\[A\-za\-z\]\|\\ )\+?),\\ \(?'state'\[A\-za\-z\]\{2\})\\ \{1,2\}?\(?'zip\_code'\(?:\[0\-9\]\{5\}\(?:\(?:\-\[0\-9\]\{4\}))?))\\r\\n))))$ |
| state | ^\(?:\[A\-za\-z\]\{2\})$ |
| street | ^\(?:\(?:\(?:\(?:\(?'apt'\[0\-9\]\{1,4\}?)\\ ))?\(?'house\_num'\(?:\[0\-9\]\|\[A\-za\-z\])\{1,8\}?)\\ \(?'street\_name'\[\!\-~\]\+?)\\r\\n))$ |
| street-name | ^\(?:\[\!\-~\]\+?)$ |
| suffix | ^\(?:\(?:\(?i)Jr\\\.\(?\-i)\|\(?i)Sr\\\.\(?\-i)\|\(?:\(?i)I\(?\-i)\|\(?i)V\(?\-i)\|\(?i)X\(?\-i))\+?))$ |
| town-name | ^\(?:\(?:\[A\-za\-z\]\|\\ )\+?)$ |
| zip-code | ^\(?:\(?:\[0\-9\]\{5\}\(?:\(?:\-\[0\-9\]\{4\}))?))$ |
| zip-part | ^\(?:\(?:\(?'town\_name'\(?:\[A\-za\-z\]\|\\ )\+?),\\ \(?'state'\[A\-za\-z\]\{2\})\\ \{1,2\}?\(?'zip\_code'\(?:\[0\-9\]\{5\}\(?:\(?:\-\[0\-9\]\{4\}))?))\\r\\n))$ |

Even for regular expressions, they look terrible. But, the idea is that you'd be using them through code ;)

Or, more specifically, through the ABNFRegexProvider that is included. For example,

```fsharp
#r "path/to/package/Fab.ABNFRegexProvider.dll"
open Fab.ABNFRegexProvider
type MyABNF = ABNFRegex< @"path/to/file/defining/ABNF/rules" >
let myMatch = MyABNF.SomeRule.Match(@"a number: 123")
printfn "%s" myMatch.["numberRule"].Value
```
