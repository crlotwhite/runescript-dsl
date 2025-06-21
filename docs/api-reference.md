# RuneScript DSL API 참조

이 문서는 RuneScript DSL 엔진의 API와 내장 함수들에 대한 참조 가이드입니다.

## 🔧 Core API

### Parser 클래스

```cpp
namespace runescript::core {

class Parser {
public:
    explicit Parser(const std::string& source);
    
    // DSL 파싱 메서드
    std::unique_ptr<ast::Program> parseRCS();
    std::unique_ptr<ast::SpellModule> parseSpell();
    std::unique_ptr<ast::RuneModule> parseRune();
    std::unique_ptr<ast::CasterProgram> parseCaster();
    
    // 오류 처리
    bool hasErrors() const;
    std::vector<ParseError> getErrors() const;
};

}
```

**사용 예제:**
```cpp
#include "runescript/core/parser.hpp"

std::string source = R"(
input = "안녕하세요"
result = input |> normalize() |> korean_g2p()
)";

runescript::core::Parser parser(source);
auto program = parser.parseRCS();

if (parser.hasErrors()) {
    for (const auto& error : parser.getErrors()) {
        std::cerr << "Parse error: " << error.message << std::endl;
    }
}
```

### Interpreter 클래스

```cpp
namespace runescript::core {

class Interpreter {
public:
    Interpreter();
    
    // 모듈 로딩
    void loadSpellModule(const std::string& filename);
    void loadRuneModule(const std::string& filename);
    
    // 실행
    Value execute(const ast::Program& program);
    Value evaluateExpression(const ast::Expr& expr);
    
    // 환경 관리
    void setVariable(const std::string& name, const Value& value);
    Value getVariable(const std::string& name) const;
};

}
```

## 📝 내장 함수 레퍼런스

### 텍스트 처리 함수

#### `normalize(text: String) -> String`
텍스트를 정규화합니다. 공백 정리, 대소문자 정규화, 특수문자 제거 등을 수행합니다.

```runescript
clean_text = "  Hello, World!  " |> normalize()
// 결과: "hello world"
```

#### `tokenize(text: String, delimiter: String = " ") -> List[String]`
텍스트를 토큰으로 분리합니다.

```runescript
tokens = "apple,banana,cherry" |> tokenize(",")
// 결과: ["apple", "banana", "cherry"]
```

#### `remove_punctuation(text: String) -> String`
구두점과 특수문자를 제거합니다.

```runescript
clean = "Hello, world!" |> remove_punctuation()
// 결과: "Hello world"
```

#### `filter_by_language(text: String, lang: String) -> String`
지정된 언어의 문자만 필터링합니다.

```runescript
korean_only = "안녕 Hello 세계" |> filter_by_language("ko")
// 결과: "안녕 세계"
```

### 한글 처리 함수

#### `hangul_to_jamo(text: String) -> String`
한글 음절을 자모 단위로 분해합니다.

```runescript
jamo = "안녕" |> hangul_to_jamo()
// 결과: "ㅇㅡ㄂ㄴㅧㄴ"
```

#### `jamo_to_hangul(text: String) -> String`
자모를 한글 음절로 결합합니다.

```runescript
hangul = "ㅇㅡ㄂ㄴㅧㄴ" |> jamo_to_hangul()
// 결과: "안녕"
```

#### `korean_g2p(text: String) -> String`
한글을 음소로 변환합니다 (Grapheme-to-Phoneme).

```runescript
phonemes = "안녕하세요" |> korean_g2p()
// 결과: "annyeong haseyo"
```

### 음성학 함수

#### `phonemize(text: String, lang: String = "auto") -> String`
텍스트를 음소 표기로 변환합니다.

```runescript
ipa = "hello world" |> phonemize("en")
// 결과: "həˈloʊ wɜːld"
```

#### `stress_marking(phonemes: String, lang: String) -> String`
음소 표기에 강세 정보를 추가합니다.

```runescript
stressed = "hello" |> phonemize("en") |> stress_marking("en")
// 결과: "həˈloʊ"
```

### 리스트 처리 함수

#### `map(list: List[T], func: Function) -> List[U]`
리스트의 각 요소에 함수를 적용합니다.

```runescript
upper_items = ["apple", "banana"] |> map(to_upper)
// 결과: ["APPLE", "BANANA"]
```

#### `filter(list: List[T], predicate: Function) -> List[T]`
조건에 맞는 요소만 필터링합니다.

```runescript
long_words = ["cat", "elephant", "dog"] |> filter(length > 4)
// 결과: ["elephant"]
```

#### `reduce(list: List[T], func: Function, initial: U) -> U`
리스트를 단일 값으로 축약합니다.

```runescript
total = [1, 2, 3, 4] |> reduce(add, 0)
// 결과: 10
```

### 유틸리티 함수

#### `split(text: String, delimiter: String) -> List[String]`
문자열을 분리합니다.

```runescript
parts = "a,b,c" |> split(",")
// 결과: ["a", "b", "c"]
```

#### `join(list: List[String], delimiter: String) -> String`
문자열 리스트를 결합합니다.

```runescript
joined = ["hello", "world"] |> join(" ")
// 결과: "hello world"
```

#### `length(item: String | List[T]) -> Number`
문자열 또는 리스트의 길이를 반환합니다.

```runescript
str_len = "hello" |> length()     // 5
list_len = [1, 2, 3] |> length()  // 3
```

### 정규식 함수

#### `match(text: String, pattern: Regex) -> List[String]`
정규식과 일치하는 부분을 찾습니다.

```runescript
matches = "hello 123 world 456" |> match(/\d+/)
// 결과: ["123", "456"]
```

#### `replace(text: String, pattern: Regex | String, replacement: String) -> String`
패턴과 일치하는 부분을 치환합니다.

```runescript
replaced = "hello world" |> replace(/l+/, "L")
// 결과: "heLo worLd"
```

## 🦄 Spell 모듈 API

### 모듈 구조

```spell
// 함수 정의
def function_name(param1, param2) = expression

// 모듈 가져오기
import "other_module.spell"

// 조건부 임포트
import "advanced.spell" if FEATURE_ENABLED
```

### 타입 시스템

RuneScript는 정적 타입 체크를 지원합니다:

- `String`: 문자열 타입
- `Regex`: 정규식 타입
- `List[T]`: 리스트 타입
- `Number`: 숫자 타입
- `Phoneme`: 음소 타입
- `Language`: 언어 타입

```spell
// 타입 지정 예제
def process_text(text: String) -> Phoneme = 
  text |> normalize() |> korean_g2p()

def count_words(text: String) -> Number = 
  text |> tokenize() |> length()
```

## 🔤 Rune 모듈 API

### 언어 정의 API

```rune
// 언어 프로파일 정의
lang "ko-KR" uses hangul_syllables, korean_phonemes
lang "en-US" uses latin_basic, english_phonemes

// 문자 그룹 정의
group vowels = ["a", "e", "i", "o", "u"]
group consonants = ["b", "c", "d", "f", "g"]

// 변환 규칙 정의
rule palatalization: /ki/ => "chi"
rule voice_assimilation: /sb/ => "zb"
```

## ⚙️ Caster 모듈 API

### 실행 설정

```caster
// 백엔드 설정
use_backend "cpp_runtime"     // C++ 런타임 사용
use_backend "python_interp"   // Python 인터프리터 사용
use_backend "wasm_vm"         // WebAssembly VM 사용

// 성능 설정
runtime {
    threads = 4
    memory_limit = "2GB"
    optimization_level = 2
}

// 실행 순서 설정
schedule {
    pipeline_order = ["preprocess", "g2p", "postprocess"]
    parallel_stages = ["g2p", "analysis"]
}
```

## 🛠️ 개발자 API

### 확장 포인트

RuneScript는 플러그인 시스템을 통해 확장 가능합니다:

```cpp
// C++ 플러그인 인터페이스
class RuneScriptPlugin {
public:
    virtual ~RuneScriptPlugin() = default;
    virtual void initialize() = 0;
    virtual Value call(const std::string& name, const std::vector<Value>& args) = 0;
    virtual std::vector<std::string> getFunctionNames() const = 0;
};
```

### 사용자 정의 함수 등록

```cpp
// C++에서 사용자 정의 함수 등록
interpreter.registerFunction("custom_g2p", [](const std::vector<Value>& args) {
    // 사용자 정의 G2P 로직
    return Value("phoneme_result");
});
```

## 📊 성능 최적화

### 캐싱 시스템

RuneScript는 자동 캐싱을 지원합니다:

```caster
runtime {
    cache_enabled = true
    cache_size = "512MB"
    cache_ttl = 3600  // 1시간
}
```

### 병렬 처리

```caster
schedule {
    parallel_stages = ["tokenization", "normalization"]
    thread_pool_size = 8
}
```

이 API 참조는 RuneScript DSL의 현재 버전을 기반으로 하며, 향후 버전에서 추가 기능과 함께 업데이트될 예정입니다.
