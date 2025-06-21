# RuneScript DSL

[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)
[![C++](https://img.shields.io/badge/C%2B%2B-20-blue.svg)](https://isocpp.org/)
[![Haskell](https://img.shields.io/badge/Haskell-GHC%209.x-purple.svg)](https://www.haskell.org/)

**RuneScript DSL**은 텍스트 처리 및 음성 합성 파이프라인을 위한 도메인 특화 언어입니다. 언어 독립적이고 선언적인 방식으로 텍스트 전처리와 음성 변환 파이프라인을 구성할 수 있도록 설계되었습니다.

## ✨ 핵심 개념

음성학적으로 인간의 언어는 각각의 용법과 규약은 다르지만, 동일한 구조의 발성 기관(성도)을 통해 생성된다는 공통점을 가집니다. 이로부터 "여러 언어를 동일한 형식의 표준으로 변환할 수 있다면 언어 제약 없이 통합된 파이프라인을 구성할 수 있다"는 아이디어에서 출발했습니다.

## 🧩 DSL 구성 요소

RuneScript는 네 가지 파일 타입으로 구성됩니다:

| 확장자 | 용도 | 설명 |
|--------|------|------|
| `.rcs` | 실행 스크립트 | 텍스트 처리 및 변환 파이프라인 정의 |
| `.spell` | 사용자 정의 함수 | G2P, 정규화 함수 등 사용자 로직 |
| `.rune` | 문자/언어 정의 | 유니코드 범위, 문자 그룹, 언어 태그 |
| `.caster` | 실행 전략 정의 | 파이프라인 흐름, 백엔드 설정, 환경 구성 |

## 🚀 빠른 시작

### 설치

```bash
# 저장소 클론
git clone https://github.com/crlotwhite/runescript-dsl.git
cd runescript-dsl

# 빌드 (C++20 컴파일러 필요)
make build

# 또는 CMake 직접 사용
mkdir build && cd build
cmake ..
make
```

### 예제 사용법

```runescript
// example.rcs
input = "안녕하세요, RuneScript입니다!"
result = input |> normalize() |> korean_g2p() |> phonemize()
output result
```

```spell
// korean_g2p.spell
def korean_g2p(text) = 
  text |> hangul_to_jamo() |> apply_phonetic_rules()
```

### 실행

```bash
# 기본 실행
./build/runescript examples/basic/hello.rcs

# 설정 파일과 함께 실행
./build/runescript --caster examples/basic/pipeline.caster examples/basic/hello.rcs

# 디버그 모드
./build/runescript --debug examples/basic/hello.rcs
```

## 🏗️ 아키텍처

- **Core Engine**: C++20 기반 고성능 파서 및 실행 엔진
- **Type Checker**: Haskell 기반 정적 타입 추론 및 검증
- **Optimizer**: IR 최적화 패스 및 성능 향상
- **Language Support**: 다국어 텍스트 처리 및 음성학적 변환

### 주요 컴포넌트

```
src/
├── core/                 # C++ 핵심 엔진
│   ├── parser.cpp       # DSL 파서
│   ├── interpreter.cpp  # 실행 엔진
│   └── ast.cpp         # AST 구현
├── main.cpp            # CLI 인터페이스

haskell/spell-checker/   # Haskell 도구
├── src/SpellChecker/
│   ├── Parser.hs       # Spell 파서
│   ├── TypeInfer.hs    # 타입 추론기
│   └── Optimizer.hs    # 최적화 패스

examples/               # 사용 예제
├── basic/             # 기본 예제
├── multilingual/      # 다국어 처리
└── speech_synthesis/  # 음성 합성
```

## 🛠️ 개발 및 빌드

### 빌드 도구

```bash
# 전체 빌드
make build

# 릴리즈 빌드
make release

# 테스트 실행
make test

# Haskell 컴포넌트 빌드
make haskell

# 예제 테스트
make test-examples

# 도움말
make help
```

### 요구사항

- **C++**: GCC 10+, Clang 11+, 또는 MSVC 2019+
- **CMake**: 3.16 이상
- **Haskell** (선택사항): GHC 9.4+ 및 Stack

## 📚 문서

- [개발 계획서](docs/development-plan.md) - 프로젝트 아키텍처 및 설계 철학
- [문법 가이드](docs/grammar-guide.md) - DSL 문법 상세 가이드
- [API 참조](docs/api-reference.md) - 내장 함수 및 API 문서
- [예제 모음](examples/README.md) - 다양한 사용 예제

## 🔧 내장 함수

### 텍스트 처리
- `normalize()` - 텍스트 정규화 (공백 정리, 대소문자 변환)
- `tokenize(delimiter)` - 텍스트 토큰화
- `remove_punctuation()` - 구두점 제거
- `length()` - 문자열 또는 리스트 길이

### 한국어 처리
- `korean_g2p()` - 한글 음소 변환
- `hangul_to_jamo()` - 한글 자모 분해
- `jamo_to_hangul()` - 자모 결합

### 음성학 함수
- `phonemize(lang)` - 음소 표기 변환
- `stress_marking(lang)` - 강세 표시

## 🌟 특징

### 파이프라인 연산자
```runescript
text |> normalize() |> tokenize() |> korean_g2p() |> phonemize()
```

### 언어 독립적 설계
```rune
// 언어별 문자 정의
define block hangul_syllables = U+AC00 .. U+D7AF
group korean_vowels = ["ㅏ", "ㅓ", "ㅗ", "ㅜ"]
lang "ko-KR" uses hangul_syllables, korean_vowels
```

### 사용자 정의 함수
```spell
def advanced_normalize(text) = 
  text |> trim() |> to_lower() |> remove_extra_spaces()

def korean_pipeline(text) = 
  text |> advanced_normalize() |> korean_g2p() |> phonemize()
```

## 🛠️ 개발 로드맵

- [x] **1단계**: 프로젝트 구조 및 문서화
- [x] **2단계**: C++ 파서 및 인터프리터 구현
- [x] **3단계**: AST 구조 및 기본 테스트
- [x] **4단계**: Haskell 타입 추론기 구현
- [x] **5단계**: 최적화 패스 및 CLI 도구
- [ ] **6단계**: 고급 음성학 기능 확장
- [ ] **7단계**: 성능 최적화 및 병렬 처리
- [ ] **8단계**: 웹 인터페이스 및 에디터 지원

## 🤝 기여하기

기여를 환영합니다! [CONTRIBUTING.md](CONTRIBUTING.md)를 참고해주세요.

### 기여 방법
1. 이슈 생성 또는 기존 이슈 확인
2. 포크 및 브랜치 생성
3. 변경사항 구현
4. 테스트 추가 및 실행
5. Pull Request 생성

## 📄 라이센스

[MIT License](LICENSE) - 자세한 내용은 LICENSE 파일을 참고하세요.

## 🙏 감사의 말

이 프로젝트는 음성 합성 및 자연어 처리 분야의 연구와 실무 경험을 바탕으로 개발되었습니다. 특히 다음 연구들에서 영감을 받았습니다:

- Tacotron 2: Natural TTS Synthesis by Conditioning WaveNet on Mel Spectrogram Predictions
- FastSpeech 2: Fast and High-Quality End-to-End Text to Speech
- VITS: Conditional Variational Autoencoder with Adversarial Learning for End-to-End Text-to-Speech

---

**RuneScript DSL**로 언어의 경계를 넘나드는 마법같은 텍스트 처리를 경험해보세요! 🎩✨
