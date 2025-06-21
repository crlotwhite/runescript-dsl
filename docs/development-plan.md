# RuneScript DSL 개발 계획서

## ✨ 개요

RuneScript DSL은 다음과 같은 개념적 배경에서 출발했다:

음성학적으로 인간의 언어는 각각의 용법과 규약은 다르지만, 동일한 구조의 발성 기관(성도)을 통해 생성된다는 공통점을 가진다. 이로부터 "여러 언어를 동일한 형식의 표준으로 변환할 수 있다면 언어 제약 없이 통합된 파이프라인을 구성할 수 있다"는 아이디어가 출발점이 되었다. 이를 기반으로, 텍스트 전처리와 음성 변환 파이프라인을 언어 독립적이고 선언적인 방식으로 구성하고자 본 DSL이 기획되었다.

프로젝트 명인 "Rune Caster"는 판타지 세계관의 마법 개념에서 착안된 것으로, 특히 언어 기반 마법을 상징하는 '룬(Rune)'과 '스펠(Spell)'이라는 개념에서 영감을 받았다. 이를 통해 인간 언어 처리 또한 마치 마법처럼 정의하고 구성할 수 있다는 상징적 의미를 담고 있다.

## 🔍 DSL 파일 구성

| 확장자 | 용도 | 주요 내용 |
|--------|------|------------|
| `.rcs` | 실행 스크립트 | 텍스트 처리 및 변환 파이프라인 정의 |
| `.spell` | 사용자 정의 함수 | 사용자 함수, G2P, 정규화 함수 등 |
| `.rune` | 문자/언어 정의 | 유니코드 범위, 문자 그룹, 언어 태그 등 |
| `.caster` | 실행 전략 정의 | 파이프라인 흐름, 백엔드 설정, 실행 환경 구성 |

## 🖋️ 문법 정의 (EBNF)

### `.rcs` 문법
```ebnf
program         ::= statement*
statement       ::= assignment | expression | import_stmt
assignment      ::= IDENTIFIER "=" expression
expression      ::= pipeline
pipeline        ::= term ("|>" term)*
term            ::= function_call | IDENTIFIER | STRING_LITERAL | REGEX_LITERAL
function_call   ::= IDENTIFIER "(" arguments? ")"
arguments       ::= expression ("," expression)*
import_stmt     ::= "import" STRING_LITERAL
```

### `.spell` 문법
```ebnf
module          ::= statement*
statement       ::= function_def | import_stmt
function_def    ::= "def" IDENTIFIER "(" parameters? ")" "=" expression
parameters      ::= IDENTIFIER ("," IDENTIFIER)*
```

### `.rune` 문법
```ebnf
module              ::= rune_stmt*
rune_stmt           ::= group_def | rule_def | unicode_range_def | lang_def
group_def           ::= "group" IDENTIFIER "=" "[" char_list "]"
char_list           ::= STRING_LITERAL ("," STRING_LITERAL)*
unicode_range_def   ::= "define block" IDENTIFIER "=" "U+" HEX ".." "U+" HEX
rule_def            ::= "rule" IDENTIFIER ":" (STRING_LITERAL | REGEX_LITERAL) "=>" STRING_LITERAL
lang_def            ::= "lang" STRING_LITERAL "uses" IDENTIFIER ("," IDENTIFIER)*
```

### `.caster` 문법
```ebnf
program         ::= directive*
directive       ::= load_stmt | run_stmt | stage_def | setting_block | backend_stmt | schedule_block

load_stmt       ::= "load" STRING_LITERAL
run_stmt        ::= "run" STRING_LITERAL
backend_stmt    ::= "use_backend" STRING_LITERAL

stage_def       ::= "stage" IDENTIFIER ":" run_stmt

setting_block   ::= "runtime" "{" setting_stmt* "}"
setting_stmt    ::= IDENTIFIER "=" VALUE

schedule_block  ::= "schedule" "{" setting_stmt* "}"
```

## 🌳 AST 구조 (C++20 기준)

```cpp
struct Expr { virtual ~Expr() = default; };
struct StringExpr : Expr { std::string value; };
struct RegexExpr  : Expr { std::string pattern; };
struct VarExpr    : Expr { std::string name; };
struct CallExpr   : Expr { std::string callee; std::vector<std::unique_ptr<Expr>> args; };
struct PipeExpr   : Expr { std::unique_ptr<Expr> lhs; std::vector<std::unique_ptr<Expr>> stages; };
struct Assignment : Expr { std::string var_name; std::unique_ptr<Expr> value; };

struct FunctionDef {
  std::string name;
  std::vector<std::string> params;
  std::unique_ptr<Expr> body;
};

struct RuleDef {
  std::string name;
  std::unique_ptr<Expr> from;
  std::string to;
};

struct GroupDef {
  std::string name;
  std::vector<std::string> characters;
};

struct UnicodeRangeDef {
  std::string name;
  uint32_t start, end;
};

struct LangProfile {
  std::string tag;
  std::vector<std::string> used_groups;
};
```

## 🧬 Haskell 적용 계획

### 적용 목적
에너자이 포지션의 자격 요건을 충족하기 위해, Haskell은 다음과 같은 영역에서 보조 도구로 활용된다:

| 적용 위치 | 역할 | 관련 기술 |
|------------|------|------------|
| DSL 파서 구현 | `.spell`, `.rune` 문법 실험 | `megaparsec` |
| 타입 추론기 | 사용자 정의 함수의 정적 타입 검사 | Hindley-Milner 방식 |
| IR 최적화 패스 | 정규화, 치환, 중복 제거 등 최적화 로직 | `optimize :: IR -> PassM IR` |
| 해석기/테스트 러너 | DSL 실행 시뮬레이터 | `eval :: Expr -> EvalM Value` |

## 🛠️ 구현 단계 로드맵

1. **파서 구현** (PEGTL 또는 Haskell `megaparsec` 기반)
2. **AST 구조 정리** 및 테스트 케이스 작성
3. **정적 타입 추론기** 설계 및 오류 리포팅 기능 개발
4. **SpellIR 정의** 및 최적화 패스 구현
5. **인터프리터** 및 CLI 구성
6. **Haskell 기반** `.spell` 정적 검사기 작성

## 📚 참고 자료

### 음성 합성 관련 논문
- Tacotron 2: Natural TTS Synthesis by Conditioning WaveNet on Mel Spectrogram Predictions
- FastSpeech 2: Fast and High-Quality End-to-End Text to Speech
- VITS: Conditional Variational Autoencoder with Adversarial Learning for End-to-End Text-to-Speech

### DSL 설계 참고
- Domain-Specific Languages by Martin Fowler
- Language Implementation Patterns by Terence Parr
- Types and Programming Languages by Benjamin Pierce
