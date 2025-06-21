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
mkdir build && cd build
cmake ..
make

# Haskell 도구 빌드 (선택적)
cd ../haskell
stack build
```

### 예제 사용법

```runescript
// example.rcs
input = "안녕하세요, RuneScript입니다!"
result = input |> normalize() |> g2p.korean() |> phonemize()
```

```spell
// korean_g2p.spell
def korean_g2p(text) = 
  text |> hangul_to_jamo() |> apply_phonetic_rules()
```

## 🏗️ 아키텍처

- **Core Engine**: C++20 기반 고성능 파서 및 실행 엔진
- **Type Checker**: Haskell 기반 정적 타입 추론 및 검증
- **Optimizer**: IR 최적화 패스 및 성능 향상
- **Language Support**: 다국어 텍스트 처리 및 음성학적 변환

## 📚 문서

- [개발 계획서](docs/development-plan.md)
- [문법 가이드](docs/grammar-guide.md)
- [API 참조](docs/api-reference.md)
- [예제 모음](examples/README.md)

## 🛠️ 개발 로드맵

- [x] **1단계**: 프로젝트 구조 및 문서화
- [ ] **2단계**: 파서 구현 (PEGTL/megaparsec)
- [ ] **3단계**: AST 구조 및 테스트 케이스
- [ ] **4단계**: 정적 타입 추론기 (Haskell)
- [ ] **5단계**: SpellIR 및 최적화 패스
- [ ] **6단계**: 인터프리터 및 CLI 도구

## 🤝 기여하기

기여를 환영합니다! [CONTRIBUTING.md](CONTRIBUTING.md)를 참고해주세요.

## 📄 라이센스

[MIT License](LICENSE) - 자세한 내용은 LICENSE 파일을 참고하세요.

## 🙏 감사의 말

이 프로젝트는 음성 합성 및 자연어 처리 분야의 연구와 실무 경험을 바탕으로 개발되었습니다.
